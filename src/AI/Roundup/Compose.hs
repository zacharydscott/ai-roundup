{-# LANGUAGE OverloadedStrings #-}

-- | Stage 4: prose.
--
-- Composition turns the resolver's decisions into the two pieces of writing the
-- corpus actually stores. For a story that already exists, "composing an update"
-- means /rewriting the standing write-up with the new coverage folded in/, and
-- separately stating the delta — what changed that day. Appending a dated
-- section instead would be cheaper and would rot: the write-up is meant to read
-- as though it were written today, and the delta is what gives the digest's
-- "additions to existing entries" section real content without a revision table.
--
-- The input is deliberately /not/ "AI.Roundup.Resolve"'s @Resolution@. Stage 3
-- decides per candidate; stage 4 works per touched story, and one story can be
-- the outcome of several candidates. 'ComposeInput' is that grouping, expressed
-- in the same types the change set uses ('StoryMeta', 'ArticleSpec'), so the
-- adapter between the two stages is a fold rather than a translation.
--
-- Every call is 'structuredChat': the write-up, the summary and the delta come
-- back as three named fields, not as prose to be split on headings. There is no
-- tool loop — the model is given the coverage and the existing write-up and has
-- nothing to look up, so a loop would only add turns for it to decline to use.

module AI.Roundup.Compose
  ( -- * Input
    ComposeInput (..)
  , ComposeTarget (..)
  , NewStoryInput (..)
  , UpdateStoryInput (..)
    -- * Composing a day
  , ComposeProgress
  , composeChangeSet
  , composeDay
  , lookupComposeSource
  , withReasoning
    -- * Rendering (exposed for inspection and tests)
  , renderArticle
  ) where

import AI.Roundup.ChangeSet
  ( ArticleSpec (..), DayChangeSet (..), DigestEntrySpec (..)
  , NewStorySpec (..), StoryCompanySpec (..), StoryMeta (..)
  , StoryUpdateSpec (..), kindBreaking, kindUpdate )
import Conspire
import Conspire.Codec.Json
       ( JSONCodec, arrayCodec, integerCodec, objectCodec, req, stringCodec )
import Conspire.Provider.Kudzu (KudzuProvider, fetchKudzuSources)
-- 'SealedStructuredChat' and 'injectSomeStructuredChat' arrive with the open
-- @Conspire@ import above, which re-exports @Conspire.TextGeneration.Wrappers@.
import Control.Concurrent.Async (waitCatch)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

-- | One story this run touched, and everything the writer needs about it.
--
-- The two constructors differ in exactly one way that matters to the prompt:
-- an update has prose to revise and a reader who has already seen it.
data ComposeTarget
  = TargetNew NewStoryInput
  | TargetUpdate UpdateStoryInput
  deriving (Eq, Show)

-- | A story that did not exist before this run. The metadata is the resolver's
-- — including 'smStartDate', which is anchored to earliest coverage and is
-- never the discovery date — and passes through composition untouched.
data NewStoryInput = NewStoryInput
  { nsiMeta     :: StoryMeta
  , nsiArticles :: [ArticleSpec]   -- ^ every article grouped under the story
  } deriving (Eq, Show)

-- | A story that already exists, with the coverage that arrived today.
--
-- 'usiSummary' and 'usiWriteup' are the /current/ stored values, read straight
-- off @story@ (see @Store.storyDetail@); the composer rewrites them rather than
-- writing beside them, so it has to be given them.
data UpdateStoryInput = UpdateStoryInput
  { usiStoryId     :: Int64
  , usiSlug        :: Text
  , usiTitle       :: Text
  , usiCategory    :: Text           -- ^ the stored category slug
  , usiSummary     :: Maybe Text
  , usiWriteup     :: Maybe Text
  , usiNewArticles :: [ArticleSpec]  -- ^ only what is new; not the whole story
  } deriving (Eq, Show)

-- | A whole run's worth of composition work, keyed by the discovery date.
data ComposeInput = ComposeInput
  { cinDate    :: Text            -- ^ discovery date, YYYY-MM-DD
  , cinTargets :: [ComposeTarget]
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Structured output shapes
--------------------------------------------------------------------------------

-- | What the model returns for one story. The same three fields whether the
-- story is new or being updated: the standing prose, and the day's delta.
data StoryProse = StoryProse
  { spSummary :: Text
  , spWriteup :: Text
  , spDelta   :: Text
  } deriving (Eq, Show)

storyProseCodec :: JSONCodec StoryProse
storyProseCodec = objectCodec "the story's prose as it stands after this run" $
  StoryProse
    <$> req "summary"
          (stringCodec "One paragraph of plain prose, no heading and no \
                       \Markdown structure: what this story is, for a reader \
                       \who has not seen it.")
    <*> req "writeup_md"
          (stringCodec "The full standing write-up in Markdown. No top-level \
                       \heading — the title is stored separately.")
    <*> req "delta_md"
          (stringCodec "What this run added, for today's digest. Two or three \
                       \sentences of Markdown, no heading.")

storyProseFormat :: Format StoryProse
storyProseFormat = Format
  { formatName = "story_prose"
  , formatCodec = storyProseCodec
  , formatDescription = Just "the standing prose for one story, plus today's delta"
  }

newtype DigestOrder = DigestOrder { doOrder :: [Int] }

digestOrderFormat :: Format DigestOrder
digestOrderFormat = Format
  { formatName = "digest_order"
  , formatCodec = objectCodec "the day's stories, most important first" $
      DigestOrder
        <$> req "order"
              (arrayCodec "every story number, most important first"
                          (integerCodec "a story number from the list"))
  , formatDescription = Just "the order the day's stories should be read in"
  }

newtype DigestIntro = DigestIntro { diIntro :: Text }

digestIntroFormat :: Format DigestIntro
digestIntroFormat = Format
  { formatName = "digest_intro"
  , formatCodec = objectCodec "the introduction to one day's digest" $
      DigestIntro
        <$> req "intro_md"
              (stringCodec "The introduction, in Markdown. One paragraph, two \
                           \at the outside. No heading.")
  , formatDescription = Just "the introduction to one day's digest"
  }

--------------------------------------------------------------------------------
-- Composing
--------------------------------------------------------------------------------

-- | Compose a whole day against a named router source.
--
-- Takes the provider rather than a resolved source so the model list is fetched
-- inside the same 'Conspiracy' run as the requests, and resolves it once for
-- the whole day rather than per story.
composeChangeSet
  :: KudzuProvider -> Text -> ComposeProgress -> ComposeInput
  -> Conspiracy DayChangeSet
composeChangeSet provider sourceName report input = do
  src <- lookupComposeSource provider sourceName
  composeDay src report input

-- | Look up a router source by name, rejecting one that cannot do structured
-- output. Which model writes the corpus stays a configuration question rather
-- than a code change — that is the whole reason this project depends on
-- Conspire rather than on an HTTP client.
lookupComposeSource :: KudzuProvider -> Text -> Conspiracy SealedStructuredChat
lookupComposeSource provider sourceName = do
  sources <- Map.mapMaybe (injectSomeStructuredChat SealedStructuredChat)
               <$> fetchKudzuSources provider
  case Map.lookup sourceName sources of
    Just src -> pure (withReasoning ReasoningLow src)
    Nothing  -> throwError . ConspiracyError $
      "no structured-output source named " <> sourceName <> " (have: "
        <> T.intercalate ", " (Map.keys sources) <> ")"

-- | Fix a source's reasoning budget.
--
-- Set here rather than on the router, because the router's routes are shared —
-- @local@ answers this program, @kudzu-agent@, and anything else pointed at the
-- same box — and a reasoning budget chosen for a hundred short triage calls has
-- no business changing what another program gets. 'modifyStandardConfig' keeps
-- the decision with the caller that has a reason for it.
--
-- Low, because nothing this program asks for is a reasoning problem: triage is
-- a relevance judgement on one short article, and composition is writing prose
-- from material it has already been handed. If write-ups come back thin, this
-- is the line to raise — and raise it only for composition, which is twenty
-- calls a run rather than several hundred.
withReasoning :: UsingStandardConfig s => ReasoningLevel -> s -> s
withReasoning level src =
  modifyStandardConfig src (\c -> c { reasoningLevel = Just level })

-- | Told about each piece of writing as it is finished: position, batch size,
-- kind, and title.
--
-- Composition is a model call per story and cannot say how long one will take,
-- so the useful thing to report is what has landed rather than what is left.
-- The digest intro is written last and is not one of the numbered stories; it
-- is reported with position @0@, kind @"intro"@, and the date as its title.
type ComposeProgress = Int -> Int -> Text -> Text -> IO ()

-- | The composed result for one target, before it is split into the change
-- set's three lists.
data Composed = Composed
  { cmpSpec     :: Either NewStorySpec StoryUpdateSpec
  , cmpTitle    :: Text
  , cmpSlug     :: Text
  , cmpCategory :: Text
  , cmpKind     :: Text
  , cmpDelta    :: Text
  }

-- | Compose every touched story, then the intro that frames them.
--
-- The intro is written last and is shown the deltas rather than the write-ups:
-- it is meant to say what the day amounted to, and the day is the deltas.
--
-- The write-ups are forked, as in "AI.Roundup.Triage" and "AI.Roundup.Resolve"
-- and for the same two reasons. The first is throughput: this is the slowest
-- stage in the run — a full write-up per story against a short classification
-- call — and it was the one stage still running strictly one request at a time,
-- leaving three of 'AI.Roundup.Main.inFlightCap'\'s four slots idle for the
-- longest part of a run. The cap still binds; the queue is one layer down in
-- conspire's manager, which is why nothing here bounds the fan-out itself.
--
-- The second is that a sequential 'forM' had no defence against an IO
-- exception. 'proseFor' catches 'ConspiracyError', but a request that times out
-- throws 'HttpException' from inside http-client, which 'catchError' cannot
-- see — so one slow story took down a run that had already paid for every story
-- before it. 'waitCatch' is what makes that cost one write-up instead.
--
-- Order is preserved, unlike triage, and has to be: 'rankForDigest' and both
-- change-set lists read this in target order. 'mapM' over the handles returns
-- in the order they were forked, so the fan-out is invisible downstream — only
-- the progress counter, which counts completions rather than positions because
-- the finishing order is now genuinely arbitrary.
composeDay
  :: StructuredChat s => s -> ComposeProgress -> ComposeInput
  -> Conspiracy DayChangeSet
composeDay src report input = do
  let targets = cinTargets input
      total   = length targets
  counter  <- liftIO (newIORef (0 :: Int))
  handles  <- mapM (forkConspiracy' . proseForTarget src) targets
  settled  <- liftIO (mapM waitCatch handles)
  composed <- mapM (uncurry (finish counter total)) (zip targets settled)
  liftIO (report 0 total "ranking" (tshow (length composed) <> " stories"))
  ordered <- rankForDigest src composed
  let entries = zipWith toEntry [1 ..] ordered
  intro <- if null ordered
             then pure Nothing
             else do
               liftIO (report 0 total "intro" (cinDate input))
               composeIntroOptional src report total (cinDate input) ordered
  pure DayChangeSet
    { dcsDate          = cinDate input
    , dcsIntro         = intro
    , dcsNewStories    = [ s | Composed { cmpSpec = Left s }  <- composed ]
    , dcsUpdates       = [ u | Composed { cmpSpec = Right u } <- composed ]
    , dcsDigestEntries = entries
    }
  where
    toEntry pos c = DigestEntrySpec
      { desSlug     = cmpSlug c
      , desKind     = cmpKind c
      , desDeltaMd  = Just (cmpDelta c)
      , desPosition = pos
      }

    -- A thread that died never returned prose, so it is treated exactly as a
    -- model that returned unusable prose: the story keeps its metadata, its
    -- articles and its place in the digest, and only the write-up falls back.
    -- The 'Left' from 'catchError' cannot arise while 'proseFor' swallows
    -- 'ConspiracyError', but it is matched rather than assumed away — the day
    -- someone gives 'proseFor' a throwing path, this stays a placeholder rather
    -- than becoming a pattern-match failure.
    finish counter total target settled = do
      let mProse = case settled of
            Right (Right p) -> p
            Right (Left _)  -> Nothing
            Left _          -> Nothing
          c = composedFrom target mProse
      n <- liftIO (atomicModifyIORef' counter (\k -> (k + 1, k + 1)))
      liftIO (report n total (cmpKind c <> failedMark mProse) (cmpTitle c))
      pure c

    failedMark (Just _) = ""
    failedMark Nothing  = " FAILED"

-- | Put the day's stories in the order a reader should meet them.
--
-- One comparative call over the whole day rather than a score per story.
-- Significance is not a property a story has on its own — it is a property of
-- where it sits among the others that day — and a model asked to rate stories
-- one at a time returns a column of sevens. This is the same reason grouping is
-- one call over the whole batch.
--
-- The ranking is what @digest_entry.position@ stores, so a page can take the
-- first five and put the rest behind an expander without ranking anything
-- itself. It also decides the order the intro writer sees them in, which is
-- why it runs first.
rankForDigest :: StructuredChat s => s -> [Composed] -> Conspiracy [Composed]
rankForDigest _ [] = pure []
rankForDigest _ [c] = pure [c]
rankForDigest src composed =
  (do (ranking, _) <- structuredChat src digestOrderFormat rankPrompt
                        [ userText (renderRankingMaterial composed) ]
      pure (reorder composed (doOrder ranking)))
    `catchError` \(ConspiracyError _) -> pure (orderForDigest composed)

-- | Apply the model's ordering without trusting it to be a permutation.
--
-- Out-of-range and repeated positions are discarded, and anything the model
-- failed to mention is appended in the fallback order. Same discipline as
-- "AI.Roundup.Group"'s assembly: a bad answer costs the ordering, never a
-- story. A digest that silently lost its last three entries because the model
-- stopped early is exactly the kind of failure nobody notices.
reorder :: [Composed] -> [Int] -> [Composed]
reorder composed order = ranked ++ filter (not . ranked') (orderForDigest composed)
  where
    indexed = zip [0 ..] composed
    picks = dedupe [ i | i <- order, i >= 0, i < length composed ]
    ranked = [ c | i <- picks, (j, c) <- indexed, i == j ]
    rankedSlugs = map cmpSlug ranked
    ranked' c = cmpSlug c `elem` rankedSlugs
    dedupe = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

-- | The fallback, and what the ranking degrades to: what broke leads, what
-- merely grew follows, each in the order the events happened.
orderForDigest :: [Composed] -> [Composed]
orderForDigest cs =
  filter ((== kindBreaking) . cmpKind) cs ++ filter ((/= kindBreaking) . cmpKind) cs

-- | Ask for one story's prose, retrying once and then giving up gracefully.
--
-- Composition is the last stage that had no failure path, and it was the
-- expensive place to lack one: a single response that was prose rather than
-- JSON threw out of 'composeDay' and cost a run 157 finished stories. The
-- retry is the same one triage uses and for the same reason — conspire's
-- 'unwrapJSON' already recovers a fenced or preamble-wrapped object, so what
-- reaches here answered in the wrong shape entirely and needs telling, not
-- asking again.
--
-- The fallback keeps the story. Its metadata, its articles and its place in
-- the digest are all already decided; only the prose is missing, and a story
-- in the corpus with a thin write-up is visible, correctable, and can be
-- rewritten by the next run that touches it. A story dropped here is coverage
-- paid for four times over and then thrown away.
proseFor :: StructuredChat s => s -> SystemPrompt -> Text -> Conspiracy (Maybe StoryProse)
proseFor src sysPrompt =
  structuredRetrying src storyProseFormat sysPrompt
    "carry the three fields summary, writeup_md and delta_md."

-- | Ask for a structured value, retrying once with the parse error quoted back,
-- and giving up with 'Nothing' rather than throwing.
--
-- Every structured call in this module goes through here, which is the point:
-- 'structuredChat' is one shot, conspire has no repair layer under it, and a
-- model that answers a request for JSON in prose is a routine outcome rather
-- than an exceptional one. Whoever adds the next structured call to this stage
-- should reach for this and not 'structuredChat' — the intro spent a while as
-- the one call that did neither, and the cost of that was a whole day's
-- finished write-ups discarded after the last of them had been paid for.
--
-- Telling rather than asking again: conspire's 'unwrapJSON' already recovers an
-- object behind a Markdown fence or a preamble, so anything that reaches this
-- handler answered in the wrong shape entirely and has to be told so. The
-- @fields@ argument is the one part that cannot be shared — it names what the
-- caller's format actually requires, and a reminder that listed the wrong
-- fields would be worse than no reminder at all.
structuredRetrying
  :: StructuredChat s
  => s -> Format f -> SystemPrompt -> Text -> Text -> Conspiracy (Maybe f)
structuredRetrying src fmt sysPrompt fields material =
    attempt (1 :: Int) [userText material]
  where
    attempt n msgs =
      (Just . fst <$> structuredChat src fmt sysPrompt msgs)
        `catchError` \(ConspiracyError msg) ->
          if n > 0
            then attempt (n - 1) (msgs ++ [userText (reformat msg)])
            else pure Nothing

    reformat msg = T.unlines
      [ "That could not be parsed: " <> T.take 200 (T.replace "\n" " " msg)
      , ""
      , "Answer again with a JSON object and nothing else — no prose before or"
      , "after it, no Markdown fence. It must start with { and end with }, and"
      , fields
      ]

-- | Prose assembled without a model, for a story whose composition failed.
--
-- Deliberately plain and obviously mechanical: it should read as a placeholder
-- to whoever sees it, not as a write-up somebody wrote badly.
fallbackProse :: Text -> [ArticleSpec] -> StoryProse
fallbackProse title arts = StoryProse
  { spSummary = title <> ". Prose generation failed for this story; what \
                \follows is its coverage as collected."
  , spWriteup = T.unlines ([ "_Automatically listed — no write-up was generated._", "" ]
                  ++ [ "- " <> asTitle a
                         <> maybe "" (" — " <>) (asOutlet a)
                         <> maybe "" (\d -> " (" <> d <> ")") (asPublishedAt a)
                     | a <- arts ])
  , spDelta = tshow (length arts) <> " article(s) collected; write-up pending."
  }

-- | The one model call a story costs. Split from 'composedFrom' so that this
-- half — the slow, failing, forkable half — is all that crosses a thread
-- boundary, and the assembly stays pure and total.
proseForTarget
  :: StructuredChat s => s -> ComposeTarget -> Conspiracy (Maybe StoryProse)
proseForTarget src (TargetNew nsi)    = proseFor src newStoryPrompt (renderNewStory nsi)
proseForTarget src (TargetUpdate usi) = proseFor src updateStoryPrompt (renderUpdate usi)

-- | A target and whatever prose came back, assembled into a change-set entry.
--
-- 'Nothing' covers both ways a write-up can be missing — a model that answered
-- unusably twice, and a thread that died before answering at all — and they
-- want the same outcome, so neither is distinguished here.
composedFrom :: ComposeTarget -> Maybe StoryProse -> Composed
composedFrom (TargetNew nsi) mProse =
  Composed
    { cmpSpec = Left NewStorySpec
        { nssMeta     = meta
        , nssSummary  = Just (spSummary prose)
        , nssWriteup  = spWriteup prose
        , nssArticles = nsiArticles nsi
        }
    , cmpTitle    = smTitle meta
    , cmpSlug     = smSlug meta
    , cmpCategory = smCategory meta
    , cmpKind     = kindBreaking
    , cmpDelta    = spDelta prose
    }
  where
    meta  = nsiMeta nsi
    prose = fromMaybe (fallbackProse (smTitle meta) (nsiArticles nsi)) mProse
composedFrom (TargetUpdate usi) mProse =
  -- An update whose prose failed keeps its existing write-up rather than
  -- overwriting it with a placeholder: 'susWriteup' and 'susSummary' are
  -- @Nothing@ below when the model gave nothing, and "AI.Roundup.Apply" only
  -- writes the fields that are present. The story keeps the prose it had and
  -- gains the new articles, which is the correct outcome and the reason those
  -- two fields are optional in the change set at all.
  Composed
    { cmpSpec = Right StoryUpdateSpec
        { susStoryId     = usiStoryId usi
        , susSlug        = usiSlug usi
        , susDeltaMd     = Just (spDelta prose)
        , susWriteup     = if failed then Nothing else Just (spWriteup prose)
        , susSummary     = if failed then Nothing else Just (spSummary prose)
        , susNewArticles = usiNewArticles usi
        }
    , cmpTitle    = usiTitle usi
    , cmpSlug     = usiSlug usi
    , cmpCategory = usiCategory usi
    , cmpKind     = kindUpdate
    , cmpDelta    = spDelta prose
    }
  where
    prose  = fromMaybe (fallbackProse (usiTitle usi) (usiNewArticles usi)) mProse
    failed = isNothing mProse

-- | The intro, or a reported 'Nothing'.
--
-- The retry is 'structuredRetrying', the same one the write-ups get; what is
-- special here is only what happens when it is exhausted. The intro is the last
-- thing a run composes and the least of what it produces: every write-up is
-- already written by the time this call is made, and the digest lists its own
-- stories whether or not a paragraph frames them. Throwing from here discarded
-- all of that before the change set reached disk — precisely the loss
-- @data\/drafts\/<date>\/changes.json@ exists to prevent — so this degrades for
-- the same reason 'fallbackProse' does, and the two should keep agreeing.
--
-- Worth knowing why it fails rather than just that it can: 'introPrompt' tells
-- the writer that "if the day was thin, say so plainly and stop", and a model
-- that takes that literally answers in prose instead of the object
-- 'digestIntroFormat' asked for. So the retry earns its place — the second ask
-- is specific about the shape where the system prompt is specific about the
-- content.
--
-- 'dcsIntro' is already a 'Maybe' and an empty day already writes 'Nothing', so
-- the absence needs no new representation. It is reported rather than swallowed:
-- a digest quietly losing its intro every day is the thing this must not become.
composeIntroOptional
  :: StructuredChat s
  => s -> ComposeProgress -> Int -> Text -> [Composed] -> Conspiracy (Maybe Text)
composeIntroOptional src report total date composed = do
  mIntro <- structuredRetrying src digestIntroFormat introPrompt
              "carry the single field intro_md."
              (renderIntroMaterial date composed)
  case mIntro of
    Just intro -> pure (Just (diIntro intro))
    Nothing -> do
      liftIO (report 0 total "intro FAILED" "unparseable after retry; digest has no intro")
      pure Nothing

--------------------------------------------------------------------------------
-- Prompts
--------------------------------------------------------------------------------

-- The instructions go in the system prompt rather than as a leading message:
-- they are identical on every call, which is what a provider's prompt cache
-- keys on, and the coverage is the only part that varies.
--
-- All three are narrow in the same direction. The failure mode of a writing
-- prompt is a model that pads — restating the feed's own blurb back at you with
-- more adjectives — so the instruction is mostly about what to leave out.

newStoryPrompt :: SystemPrompt
newStoryPrompt = T.unlines
  [ "You are writing the standing entry for a story that has just entered a"
  , "corpus of AI news. You are given the story's metadata and the article"
  , "coverage it was grouped from: titles, outlets, dates and excerpts."
  , ""
  , "Say what actually happened. Attribute a claim to the outlet that made it"
  , "when the outlets disagree, and prefer the vendor's own wording for what a"
  , "thing is over a reporter's paraphrase of it."
  , ""
  , "Do not pad. If the coverage supports three sentences, write three. Do not"
  , "restate a headline with adjectives added, do not explain why the story"
  , "matters unless a source says why, and do not editorialise about the"
  , "industry. Never invent detail that is not in the material given — the"
  , "excerpts are feed summaries and are often truncated, and guessing at the"
  , "rest of an article is worse than omitting it."
  , ""
  , "The delta is what today's digest will print under this story. Write it for"
  , "someone scanning the digest: what broke, in two or three sentences. It is"
  , "not a teaser for the write-up."
  ]

updateStoryPrompt :: SystemPrompt
updateStoryPrompt = T.unlines
  [ "You are updating the standing entry for a story that is already in a"
  , "corpus of AI news. You are given the existing write-up and only the"
  , "coverage that arrived today."
  , ""
  , "Rewrite the write-up with the new coverage folded in — not appended. The"
  , "result must read as though it had been written today from nothing: no"
  , "\"Update:\" sections, no dated log entries, no visible seam where the new"
  , "material was joined."
  , ""
  , "Keep everything the existing write-up says that the new coverage does not"
  , "contradict. You are revising, not rewriting to taste, and dropping"
  , "material because you would have phrased it differently loses coverage the"
  , "reader already had. Where a new article contradicts the existing text, say"
  , "which outlet reports what rather than picking a winner."
  , ""
  , "The delta is written for a reader who read yesterday's version: say what"
  , "is new, not what the story is. Do not re-summarise the story there. If"
  , "today's coverage adds nothing of substance — a second outlet repeating the"
  , "first — say that in one sentence rather than inflating it."
  , ""
  , "Revise the summary only if the new coverage changes what the story is."
  , "Never invent detail that is not in the material given."
  ]

introPrompt :: SystemPrompt
introPrompt = T.unlines
  [ "You are writing the introduction to a daily digest of AI news, read by the"
  , "people who decide how AI gets adopted inside a company. You are given every"
  , "story the digest covers — which broke today, which were already running and"
  , "gained coverage — in the order they will be shown, with the day's delta for"
  , "each."
  , ""
  , "Say what the day amounted to for that reader: what changed about what they"
  , "can buy, what it costs, what they are permitted to do with it, or what their"
  , "market just did. Name the two or three things that actually matter and say"
  , "why they do; the digest lists every story itself, directly below you, so"
  , "listing them again is wasted."
  , ""
  , "Weigh the day the way the ordering does — commercial, regulatory and"
  , "deployment news first, technology by what it changes for a buyer, and"
  , "consumer or local-AI curiosities not at all. The stories arrive already"
  , "ranked, so the ones at the top are the ones to write about."
  , ""
  , "Do not open with \"Today in AI\" or any variant of it, do not close with a"
  , "look ahead, and do not describe the digest to its reader. If the day was"
  , "thin, say so plainly and stop — a short introduction to a thin day is"
  , "correct, and padding it is not."
  ]

--------------------------------------------------------------------------------
-- Rendering the material the model reads
--------------------------------------------------------------------------------

-- Plain labelled lines rather than JSON: the model reads this and nothing
-- parses it, so JSON would only spend tokens on syntax.

-- | One article as the writer sees it.
-- | Every story of the day, numbered, with what changed about it. The deltas
-- rather than the write-ups: ranking is about what happened today, and a
-- standing write-up describes months.
--
-- The category rides along because it is the one signal the lens keys on that a
-- title and a delta do not reliably carry. @regulation@ and @pricing@ are the
-- two the ranking is most often asked to promote, and a title can describe
-- either without using a word that says so.
renderRankingMaterial :: [Composed] -> Text
renderRankingMaterial composed = T.unlines
  [ T.unlines
      [ "[" <> tshow i <> "] " <> cmpTitle c
      , "    " <> (if cmpKind c == kindBreaking then "new story" else "update")
          <> ", " <> cmpCategory c <> ": " <> cmpDelta c
      ]
  | (i, c) <- zip [0 ..] composed
  ]

rankPrompt :: SystemPrompt
rankPrompt = T.unlines
  [ "You are ordering one day's AI stories for the people who decide how AI gets"
  , "adopted inside a company — the ones who choose vendors, sign contracts,"
  , "approve budgets, answer to legal, and have to tell an executive on Monday"
  , "what changed. They are not building models. They are buying, deploying and"
  , "governing them."
  , ""
  , "Return every story number, most important first. The first five are shown on"
  , "their own, so the top of the list carries most of the weight: put there what"
  , "that reader would be embarrassed not to have heard about."
  , ""
  , "Lead with the commercial and legal reality of running AI in a business:"
  , ""
  , "  - Enterprise business news: contracts and partnerships, acquisitions,"
  , "    funding at scale, market entries and exits, a vendor winning or losing a"
  , "    named customer, infrastructure commitments large enough to change who"
  , "    can serve whom."
  , "  - Regulation and legal exposure: new rules, enforcement actions, court"
  , "    rulings, compliance deadlines — anything that changes what a company is"
  , "    permitted to deploy or obliged to disclose."
  , "  - Enterprise commercial terms: pricing, licensing, tiers, quotas, rate"
  , "    limits, contract and data-use terms. Anything that changes the bill or"
  , "    the terms it arrives under."
  , "  - Business cases with a name on them: an identifiable organisation putting"
  , "    AI into production, with enough detail to learn from — what they"
  , "    deployed, at what scale, and to what effect."
  , ""
  , "Then technology, judged by what it changes for a buyer rather than by what"
  , "it demonstrates: a model, product or platform capability that is available"
  , "now, what it costs, what it makes newly possible, what it replaces. A"
  , "capability that shipped with a price attached outranks a benchmark result"
  , "nobody can act on yet. Security and safety findings belong here when they"
  , "have a real blast radius — a vulnerability in something companies are"
  , "already running is business news, not research."
  , ""
  , "Rank down:"
  , ""
  , "  - Local and on-device AI: consumer hardware, hobbyist self-hosting,"
  , "    running models on a laptop or a phone. Note the distinction that"
  , "    matters: self-hosting at enterprise scale — a vendor's models deployed"
  , "    in a company's own cloud or datacentre, with licensing and support"
  , "    attached — is a procurement story and belongs near the top."
  , "  - Consumer apps and features with no business edition."
  , "  - Research with no near-term bearing on what is shipping."
  , "  - Incremental versions, minor features, and a company's internal affairs"
  , "    with no effect outside its own walls."
  , ""
  , "Judge by significance to that reader, not by novelty. A substantial update"
  , "to a story already running outranks a minor new one — being new is not the"
  , "same as mattering. Include every number exactly once."
  ]

renderArticle :: ArticleSpec -> Text
renderArticle a = T.intercalate "\n"
  [ "  TITLE: " <> asTitle a
  , "  OUTLET: " <> fromMaybe "(unknown)" (asOutlet a)
  , "  PUBLISHED: " <> fromMaybe "(undated)" (asPublishedAt a)
  , "  URL: " <> asUrlCanonical a
  , "  EXCERPT: " <> fromMaybe "(none)" (asExcerpt a)
  ]

-- | Oldest coverage first, undated coverage last. The order is the order the
-- events happened in, which is the order the write-up wants to narrate.
renderArticles :: [ArticleSpec] -> Text
renderArticles =
  T.intercalate "\n\n"
  . map renderArticle
  . sortOn (\a -> (isNothing (asPublishedAt a), asPublishedAt a))

renderNewStory :: NewStoryInput -> Text
renderNewStory nsi = T.intercalate "\n" $
  [ "STORY: " <> smTitle meta
  , "CATEGORY: " <> smCategory meta
  , "COMPANIES: " <> renderCompanies (smCompanies meta)
  ] ++
  [ "MODEL FAMILY: " <> fam | Just fam <- [smModelFamily meta] ] ++
  [ "EARLIEST COVERAGE: " <> smStartDate meta <> " (" <> smStartBasis meta <> ")"
  , ""
  , "COVERAGE (" <> tshow (length (nsiArticles nsi)) <> "):"
  , renderArticles (nsiArticles nsi)
  ]
  where meta = nsiMeta nsi

renderUpdate :: UpdateStoryInput -> Text
renderUpdate usi = T.intercalate "\n"
  [ "STORY: " <> usiTitle usi
  , ""
  , "EXISTING SUMMARY:"
  , fromMaybe "(none)" (usiSummary usi)
  , ""
  , "EXISTING WRITE-UP:"
  , fromMaybe "(none)" (usiWriteup usi)
  , ""
  , "NEW COVERAGE TODAY (" <> tshow (length (usiNewArticles usi)) <> "):"
  , renderArticles (usiNewArticles usi)
  ]

renderIntroMaterial :: Text -> [Composed] -> Text
renderIntroMaterial date composed = T.intercalate "\n\n" $
  ("DIGEST DATE: " <> date) : map one composed
  where
    one c = T.intercalate "\n"
      [ label (cmpKind c) <> ": " <> cmpTitle c
      , cmpDelta c
      ]
    label k | k == kindBreaking = "BROKE TODAY"
            | otherwise         = "ADDED TO AN EXISTING STORY"

renderCompanies :: [StoryCompanySpec] -> Text
renderCompanies [] = "(none)"
renderCompanies cs = T.intercalate ", " (map name cs)
  where name c = scsName c <> if scsPrimary c then " (primary)" else ""

tshow :: Int -> Text
tshow = T.pack . show
