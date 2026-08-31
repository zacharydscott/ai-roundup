{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Stage 5: is this group new to the corpus, or more coverage of something
-- already in it?
--
-- One sub-agent per group, with two read-only tools over the corpus and a
-- third through which the answer arrives. Two outcomes only: a story the
-- corpus does not have, or an existing story this adds to.
--
-- __What changed, and why it is smaller.__ This used to run per /candidate/,
-- sequentially, and carry a @RunLedger@ of the stories minted so far so that
-- candidate /n/ could see what candidate /n-1/ created. That machinery existed
-- for one failure: two articles about one brand-new story becoming two stories.
-- "AI.Roundup.Group" now fixes the partition before this stage runs, so the
-- failure cannot be expressed — one group is one story by construction. The
-- ledger, the pending list, the search-before-new rule and the pending-slug
-- variant of a story reference all went with it.
--
-- Two more outcomes went too. @ignore@ belonged to relevance, which
-- "AI.Roundup.Triage" now settles far more cheaply. @duplicate@ belonged to
-- "this exact article is already stored", which is a canonical-URL lookup
-- costing nothing — 'alreadyStored' does it before a model is asked anything.
-- What is left here is the one question that genuinely needs the corpus and a
-- judgement: has this event been written about before?
--
-- __Groups resolve in parallel.__ Once the partition is fixed the groups are
-- independent — nothing one decides changes what another should. The single
-- shared resource is the slug namespace, and slugs are de-conflicted after the
-- fan-out rather than serialised through it, because a collision is cheap to
-- repair and impossible to prevent concurrently.

module AI.Roundup.Resolve
  ( -- * Output
    Resolution (..)
  , ResolvedGroup (..)
    -- * Running the resolver
  , ResolveProgress
  , resolveGroups
  , alreadyStored
    -- * Model access
  , resolverSource
  , resolverProvider
  , routerKeyEnv
  , defaultRouterUrl
  ) where

import AI.Roundup.ChangeSet (ArticleSpec (..), StoryCompanySpec (..), StoryMeta (..))
import AI.Roundup.Compose (withReasoning)
import AI.Roundup.Group (CandidateGroup (..), groupArticles)
import AI.Roundup.Ingest (Candidate (..))
import AI.Roundup.Store
       ( Store, ArticleRow (..), CategoryRow (..), ListFilter (..)
       , StoryCompanyRow (..), StoryDetailRow (..), StoryListItem (..)
       , StoryQuery (..), StoryRelationRow (..), StorySummaryRow (..)
       , articleForUrl, categories, listStories, storiesMatching, storyDetail )
import AI.Roundup.Triage (CandidateSummary (..), Triaged (..))

import Conspire
import Conspire.Codec.Json
       ( JSONCodec, arrayCodec, boolCodec, enumCodec, integerCodec, objectCodec
       , opt, req, stringCodec )
import Conspire.Provider (filterToolChats)
import Conspire.Provider.Kudzu (KudzuProvider, fetchKudzuSources, newKudzuFromEnv)
import Conspire.Tool (TextTool, mkTextTool)

import Control.Concurrent.Async (waitCatch)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum)
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

-- | What one group turned out to be.
--
-- No @Maybe Int64@ anywhere, and no pending variant: every id here came out of
-- the database during this stage, so it exists. Stories minted this run have
-- no id and are named by the 'StoryMeta' that will create them.
data Resolution
  = New StoryMeta  -- ^ no story covers this event; metadata decided here
  | Update Int64   -- ^ more coverage of the story with this id
  deriving (Eq, Show)

data ResolvedGroup = ResolvedGroup
  { rgGroup      :: CandidateGroup
  , rgResolution :: Resolution
  } deriving (Eq, Show)

-- | Told about each group as it finishes: completion count, batch size,
-- outcome. A running total of completions, not a position — the fan-out
-- finishes out of order.
type ResolveProgress = Int -> Int -> ResolvedGroup -> IO ()

--------------------------------------------------------------------------------
-- Run state
--------------------------------------------------------------------------------

-- | What one group's sub-agent has actually been shown.
--
-- Ids exist only in the corpus, so the only way one reaches the model is out
-- of a tool result. Recording what went out is enough to tell a real id from
-- an invented one, and it has to be done this way: "AI.Roundup.Store"
-- deliberately exposes no id-existence lookup for the resolver to lean on.
newtype Seen = Seen { seenStories :: Set.Set Int64 }

data ResolveEnv = ResolveEnv
  { reStore      :: Store
  , reSource     :: SealedToolChat
  , reCategories :: [Text]  -- ^ the seeded category slugs; the only legal values
  }

--------------------------------------------------------------------------------
-- The free pass
--------------------------------------------------------------------------------

-- | Is this candidate's article already in the corpus?
--
-- A canonical-URL lookup, no model involved, run before triage rather than
-- after grouping — an article the corpus already has should not cost even the
-- cheap call. This is what makes rerunning a day free: the second run finds
-- every article present and asks nothing of any model.
alreadyStored :: Store -> Candidate -> IO Bool
alreadyStored store cand =
  maybe False (const True)
    <$> articleForUrl store (asUrlCanonical (cndArticle cand))

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Resolve every group against the corpus, in parallel.
--
-- Slugs are de-conflicted afterwards rather than during: two groups minting
-- the same slug concurrently cannot be prevented without serialising the whole
-- stage, and the repair is one pass over the results. Left unrepaired it would
-- be silent and bad — "AI.Roundup.Apply" treats a slug that already exists as
-- the same story, so two genuinely different stories sharing one slug would
-- merge into a single row.
resolveGroups
  :: Store -> SealedToolChat -> ResolveProgress -> [CandidateGroup]
  -> Conspiracy [ResolvedGroup]
resolveGroups _ _ _ [] = pure []
resolveGroups store src report groups = do
  cats  <- map categorySlug <$> liftIO (categories store)
  slugs <- map litSlug <$> liftIO (listStories store (ListFilter Nothing Nothing Nothing 100000))
  let env = ResolveEnv store src cats
      total = length groups
  counter <- liftIO (newIORef (0 :: Int))
  handles <- mapM (forkConspiracy' . one env counter total) groups
  -- 'waitCatch', never 'wait': see the note in "AI.Roundup.Triage". An HTTP
  -- timeout is an IO exception, invisible to 'catchError', and 'wait' would
  -- rethrow it here and lose every group already resolved.
  settled <- liftIO (mapM waitCatch handles)
  resolved <- mapM (uncurry (recover env counter total)) (zip groups settled)
  pure (deconflict (Set.fromList slugs) resolved)
  where
    -- A group whose sub-agent errors is treated as new rather than dropped.
    -- The article is real coverage either way, and a wrongly-new story is
    -- visible in the corpus and fixable; a dropped one is neither.
    one env counter total grp =
      (do res <- resolveOne env grp
          finish counter total (ResolvedGroup grp res))
        `catchError` \(ConspiracyError _) ->
          finish counter total (ResolvedGroup grp (New (fallbackMeta env grp)))

    recover env counter total grp settled = case settled of
      Right (Right r) -> pure r
      _ -> finish counter total (ResolvedGroup grp (New (fallbackMeta env grp)))

    finish counter total resolved = do
      n <- liftIO $ atomicModifyIORef' counter (\k -> (k + 1, k + 1))
      liftIO (report n total resolved)
      pure resolved

-- | Make every minted slug unique, against the corpus and against each other.
deconflict :: Set.Set Text -> [ResolvedGroup] -> [ResolvedGroup]
deconflict corpus = snd . foldl step (corpus, [])
  where
    step (taken, acc) r = case rgResolution r of
      Update _ -> (taken, acc ++ [r])
      New meta ->
        let slug = free taken (smSlug meta)
        in ( Set.insert slug taken
           , acc ++ [ r { rgResolution = New meta { smSlug = slug } } ] )

    free taken wanted = go (0 :: Int)
      where
        go n =
          let s = if n == 0 then wanted else wanted <> "-" <> T.pack (show (n + 1))
          in if Set.member s taken then go (n + 1) else s

-- | Metadata for a group whose sub-agent never answered. Everything comes off
-- the group itself — no model, no invention — so the coverage lands somewhere
-- readable and correctable rather than nowhere.
fallbackMeta :: ResolveEnv -> CandidateGroup -> StoryMeta
fallbackMeta env grp = StoryMeta
  { smSlug        = fromMaybe "untitled-story" (nonEmpty (slugify (cgTitle grp)))
  , smTitle       = cgTitle grp
  , smCategory    = if cgCategory grp `elem` reCategories env
                      then cgCategory grp else "general_tech"
  , smModelFamily = Nothing
  , smCompanies   = []
  , smStartDate   = anchorDate grp
  , smStartBasis  = "earliest_coverage"
  }

--------------------------------------------------------------------------------
-- One group
--------------------------------------------------------------------------------

-- | Maximum times the sub-agent is re-prompted after finishing a turn without
-- recording a decision.
maxAttempts :: Int
maxAttempts = 3

resolveOne :: ResolveEnv -> CandidateGroup -> Conspiracy Resolution
resolveOne env grp = do
  seenRef     <- liftIO (newIORef (Seen Set.empty))
  decisionRef <- liftIO (newIORef Nothing)

  let anchor = anchorDate grp
      tools  = Map.fromList
        [ ("search_stories",    searchStoriesTool (reStore env) seenRef)
        , ("get_story",         getStoryTool (reStore env) seenRef)
        , ("record_resolution", recordResolutionTool env grp anchor seenRef decisionRef)
        ]
      stopWhenDecided = fmap (fmap (const ())) (liftIO (readIORef decisionRef))

  let go 0 _ = pure ()
      go n msgs = do
        (comps, _) <- toolChatUntil stopWhenDecided (reSource env) tools resolverPrompt msgs
        decided <- liftIO (readIORef decisionRef)
        case decided of
          Just _  -> pure ()
          Nothing -> go (n - 1)
            (msgs ++ map AgentMessage (NE.toList comps) ++ [userText nudge])

  go maxAttempts [userText (groupBrief anchor grp)]
  -- A sub-agent that never answers gets the same treatment as one that errored:
  -- the group is new. See 'resolveGroups'.
  fromMaybe (New (fallbackMeta env grp)) <$> liftIO (readIORef decisionRef)

nudge :: Text
nudge = T.unlines
  [ "You ended your turn without calling record_resolution."
  , "Call it now. This group is either a new story or an update to an existing"
  , "one — those are the only two answers, and declining to answer is not one."
  ]

--------------------------------------------------------------------------------
-- The group brief
--------------------------------------------------------------------------------

-- | What the sub-agent starts with: the event, and every article covering it.
groupBrief :: Text -> CandidateGroup -> Text
groupBrief anchor grp = T.unlines $
  [ "EVENT"
  , "proposed title: " <> cgTitle grp
  , "category:       " <> cgCategory grp
  , ""
  , "COVERAGE (" <> tshow (length (cgMembers grp)) <> " article(s))"
  ]
  ++ concat
     [ [ "  - " <> asTitle art
       , "    " <> fromMaybe "(unknown outlet)" (asOutlet art)
           <> ", " <> fromMaybe "no date" (asPublishedAt art)
       , "    " <> csSummary (tgSummary t)
       ]
     | t <- cgMembers grp
     , let art = cndArticle (tgCandidate t)
     ]
  ++ [ ""
     , "EARLIEST COVERAGE ANCHOR: " <> anchor
     , "This is min(published_at) across the whole group. It is the story's"
     , "start_date unless a source states the event date itself."
     ]

--------------------------------------------------------------------------------
-- Tool: search_stories
--------------------------------------------------------------------------------

searchStoriesTool :: Store -> IORef Seen -> TextTool
searchStoriesTool store seenRef = mkTextTool
  "Search the story corpus. All filters are optional; omit them all to see the\
  \ most recently updated stories. Free text matches title and summary as a\
  \ substring, so prefer short distinctive phrases (a model name, a regulation)\
  \ over sentences, and search more than once with different wordings before\
  \ concluding a story is new."
  codec
  where
    codec :: JSONCodec (Conspiracy Value)
    codec = fmap runSearch $ objectCodec "search_stories arguments" $
      (,,,) <$> opt "query"    (stringCodec "free text matched against title and summary")
            <*> opt "company"  (stringCodec "company name, slug or alias")
            <*> opt "category" (stringCodec "category slug")
            <*> opt "since"    (stringCodec "ISO date; only stories updated on or after it")

    runSearch (mQuery, mCompany, mCategory, mSince) = do
      rows <- liftIO $ storiesMatching store StoryQuery
        { sqText = mQuery, sqCompany = mCompany, sqCategory = mCategory
        , sqSince = mSince, sqLimit = 10 }
      liftIO $ atomicModifyIORef' seenRef $ \s ->
        ( s { seenStories = foldr (Set.insert . ssrId) (seenStories s) rows }, () )
      pure $ object [ "stories" .= map summaryJson rows ]

    summaryJson r = object
      [ "id"           .= ssrId r
      , "slug"         .= ssrSlug r
      , "title"        .= ssrTitle r
      , "category"     .= ssrCategory r
      , "companies"    .= ssrCompanies r
      , "start_date"   .= ssrStartDate r
      , "last_updated" .= ssrLastUpdated r
      , "summary"      .= fmap (clip 400) (ssrSummary r)
      ]

--------------------------------------------------------------------------------
-- Tool: get_story
--------------------------------------------------------------------------------

getStoryTool :: Store -> IORef Seen -> TextTool
getStoryTool store seenRef = mkTextTool
  "Fetch one story in full by its numeric id, including its article list. Use\
  \ it when a search hit looks like it might be this event and you need the\
  \ detail to be sure."
  codec
  where
    codec :: JSONCodec (Conspiracy Value)
    codec = fmap runGet $ objectCodec "get_story arguments" $
      req "story_id" (integerCodec "the numeric id of a story from search_stories")

    runGet sid = do
      detail <- liftIO $ storyDetail store (fromIntegral sid)
      case detail of
        Nothing -> pure $ object [ "error" .= ("no story with id " <> tshow sid) ]
        Just d -> do
          liftIO $ atomicModifyIORef' seenRef $ \s ->
            ( s { seenStories = Set.insert (sdrId d) (seenStories s) }, () )
          pure (detailJson d)

    detailJson d = object
      [ "id"               .= sdrId d
      , "slug"             .= sdrSlug d
      , "title"            .= sdrTitle d
      , "category"         .= sdrCategory d
      , "model_family"     .= sdrModelFamily d
      , "summary"          .= sdrSummary d
      , "writeup"          .= fmap (clip 2000) (sdrWriteup d)
      , "start_date"       .= sdrStartDate d
      , "start_date_basis" .= sdrStartBasis d
      , "last_updated"     .= sdrLastUpdated d
      , "companies"        .= [ object [ "name" .= scrName c, "is_primary" .= scrPrimary c ]
                              | c <- sdrCompanies d ]
      , "related"          .= [ object [ "slug" .= srrRelatedSlug r
                                       , "relation" .= srrRelation r
                                       , "note" .= srrNote r ]
                              | r <- sdrRelations d ]
      , "articles"         .= [ object [ "article_id" .= artId a
                                       , "url" .= artUrlCanonical a
                                       , "title" .= artTitle a
                                       , "outlet" .= artOutlet a
                                       , "published_at" .= artPublishedAt a ]
                              | a <- sdrArticles d ]
      ]

--------------------------------------------------------------------------------
-- Tool: record_resolution
--------------------------------------------------------------------------------

data Decision = Decision
  { dnKind    :: Text
  , dnStoryId :: Maybe Int
  , dnStory   :: Maybe Draft
  }

-- | Proposed metadata for a new story. Separate from 'StoryMeta' because the
-- dates here are advisory: 'anchorStart' decides what actually gets stored.
data Draft = Draft
  { dfSlug        :: Text
  , dfTitle       :: Text
  , dfCategory    :: Text
  , dfModelFamily :: Maybe Text
  , dfCompanies   :: [StoryCompanySpec]
  , dfStartDate   :: Maybe Text
  , dfStartBasis  :: Maybe Text
  }

-- | Every rejection returns text the model can act on rather than throwing, so
-- a wrong category slug or an invented story id costs one turn instead of the
-- group. The decision ref is written only on success, which is also what the
-- loop's stop signal reads — so "the loop ended" and "a valid decision exists"
-- cannot disagree.
recordResolutionTool
  :: ResolveEnv -> CandidateGroup -> Text
  -> IORef Seen -> IORef (Maybe Resolution)
  -> TextTool
recordResolutionTool env grp anchor seenRef decisionRef = mkTextTool
  "Record the decision for this event. Call this exactly once, as the last\
  \ thing you do. 'new' needs the story object; 'update' needs story_id."
  codec
  where
    codec :: JSONCodec (Conspiracy Value)
    codec = fmap runRecord $ objectCodec "record_resolution arguments" $
      Decision
        <$> req "decision" (enumCodec "what this event is" (["new", "update"] :: [Text]))
        <*> opt "story_id" (integerCodec "id of the existing story, for 'update'")
        <*> opt "story"    draftCodec

    draftCodec :: JSONCodec Draft
    draftCodec = objectCodec "metadata for a brand-new story" $
      Draft
        <$> req "slug"     (stringCodec "short hyphenated identifier, e.g. 'openai-gpt5-release'")
        <*> req "title"    (stringCodec "headline for the story as a whole, not for one article")
        <*> req "category" (stringCodec ("one of: " <> T.intercalate ", " (reCategories env)))
        <*> opt "model_family" (stringCodec "model family slug, e.g. 'gpt-5', if this is about one")
        <*> req "companies" (arrayCodec "companies involved; exactly one is primary" companyCodec)
        <*> opt "start_date" (stringCodec
              "YYYY-MM-DD, ONLY when a source states the event date. Omit otherwise.")
        <*> opt "start_date_basis" (enumCodec
              "'stated_in_source' if start_date comes from a source saying so, else 'earliest_coverage'"
              (["earliest_coverage", "stated_in_source"] :: [Text]))

    companyCodec :: JSONCodec StoryCompanySpec
    companyCodec = objectCodec "a company involved in the story" $
      StoryCompanySpec
        <$> req "name" (stringCodec "the company's usual name, e.g. 'OpenAI'")
        <*> req "is_primary" (boolCodec "true for the one company the story is mainly about")

    runRecord decision = do
      seen <- liftIO (readIORef seenRef)
      case validate env grp anchor seen decision of
        Left err -> pure $ object [ "recorded" .= False, "error" .= err ]
        Right res -> do
          liftIO $ writeIORef decisionRef (Just res)
          pure $ object [ "recorded" .= True, "as" .= describe res ]

    describe res = case res of
      New meta -> "new story " <> smSlug meta <> " starting " <> smStartDate meta
                  <> " (" <> smStartBasis meta <> ")"
      Update i -> "update to story " <> tshow i

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validate :: ResolveEnv -> CandidateGroup -> Text -> Seen -> Decision -> Either Text Resolution
validate env grp anchor seen d = case dnKind d of
  "new" -> do
    draft <- note "decision 'new' requires the 'story' object." (dnStory d)
    let slug = slugify (dfSlug draft)
    failIf (T.null slug) "'story.slug' is empty after normalisation."
    failIf (dfCategory draft `notElem` reCategories env) $
      "'" <> dfCategory draft <> "' is not a category. Use one of: "
        <> T.intercalate ", " (reCategories env) <> "."
    let (startDate, basis) = anchorStart anchor (dfStartDate draft) (dfStartBasis draft)
    pure $ New StoryMeta
      { smSlug        = slug
      , smTitle       = orElse (T.strip (dfTitle draft)) (cgTitle grp)
      , smCategory    = dfCategory draft
      , smModelFamily = fmap slugify (nonEmpty =<< dfModelFamily draft)
      , smCompanies   = onePrimary (dfCompanies draft)
      , smStartDate   = startDate
      , smStartBasis  = basis
      }

  "update" -> do
    sid <- fromIntegral <$> note "decision 'update' requires 'story_id'." (dnStoryId d)
    -- An invented story id survives all the way to the applier, where it
    -- becomes articles hung off a story that does not exist.
    failIf (not (Set.member sid (seenStories seen))) $
      "Story id " <> tshow sid <> " has not appeared in any tool result. Search \
      \for the story and use an id from the results, or fetch it with get_story."
    pure (Update sid)

  other -> Left ("unknown decision '" <> other <> "'.")
  where
    note msg = maybe (Left msg) Right
    failIf cond msg = if cond then Left msg else Right ()

--------------------------------------------------------------------------------
-- Dates
--------------------------------------------------------------------------------

-- | Where @start_date@ actually comes from.
--
-- The anchor wins by default and the model only gets to move it earlier, and
-- only by saying so explicitly. Date extraction is the least reliable step in
-- this pipeline, and every failure mode it has — today's date, the crawl date,
-- a date invented to fill a required field — makes the story later than it is,
-- so the one direction never accepted is /later than the earliest coverage/.
anchorStart :: Text -> Maybe Text -> Maybe Text -> (Text, Text)
anchorStart anchor mDate mBasis = fromMaybe (anchor, "earliest_coverage") $ do
  basis <- mBasis
  if basis /= "stated_in_source" then Nothing else do
    stated <- nonEmpty =<< mDate
    _ <- parseDay stated
    if T.take 10 stated <= T.take 10 anchor
      then Just (stated, "stated_in_source")
      else Nothing

parseDay :: Text -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack . T.take 10

-- | min(published_at) over every article in the group and every collapsed copy
-- of them, falling back to our own clock only when no feed gave a date at all.
anchorDate :: CandidateGroup -> Text
anchorDate grp = case mapMaybe asPublishedAt arts of
  []   -> fromMaybe "" (fmap asFetchedAt (headMay arts))
  pubs -> minimum pubs
  where
    arts = groupArticles grp
    headMay []    = Nothing
    headMay (x:_) = Just x

-- | Exactly one primary company, decided here rather than asked for again.
onePrimary :: [StoryCompanySpec] -> [StoryCompanySpec]
onePrimary specs =
  let named = [ c { scsName = T.strip (scsName c) } | c <- specs, not (T.null (T.strip (scsName c))) ]
      primary = fromMaybe 0 (findIndex scsPrimary named)
  in zipWith (\i c -> c { scsPrimary = i == primary }) [0 :: Int ..] named

--------------------------------------------------------------------------------
-- The system prompt
--------------------------------------------------------------------------------

resolverPrompt :: SystemPrompt
resolverPrompt = T.unlines
  [ "You are the resolver for a corpus of AI news stories. A story is an event"
  , "that accumulates coverage over time — a model release, a regulation, a"
  , "funding round — and each story collects articles from many outlets."
  , ""
  , "You are given one event, already grouped: every article below covers the"
  , "same occurrence. Decide which of two things it is, then call"
  , "record_resolution exactly once:"
  , ""
  , "  new     no story in the corpus covers this event. You supply the story's"
  , "          metadata: slug, title, category, companies."
  , "  update  a story in the corpus already covers this event, and these"
  , "          articles are more coverage of it. Give its story_id."
  , ""
  , "Search before concluding anything is new. search_stories matches title and"
  , "summary as a substring, so one query proves little — try the model name,"
  , "the company, the category."
  , ""
  , "An existing story about the same company is not the same story. A new"
  , "version of a model is a new story, not an update to the story about the"
  , "previous version. Update means these articles are about the event that"
  , "story already describes."
  , ""
  , "On start_date: the story's date is when the event happened, never when we"
  , "found it. Leave start_date out and it is set to the earliest coverage in"
  , "the group, which is almost always right. Only set it, with"
  , "start_date_basis 'stated_in_source', when a source actually states the"
  , "date — 'released on Tuesday the 12th', 'takes effect 2 August'. Never set"
  , "it later than the earliest coverage."
  ]

--------------------------------------------------------------------------------
-- Model access
--------------------------------------------------------------------------------

-- | The router key lives in the environment, never in a file the repo tracks.
routerKeyEnv :: Text
routerKeyEnv = "KUDZU_API_KEY"

defaultRouterUrl :: Text
defaultRouterUrl = "https://gx10-da00.local:2620"

-- | The provider, or 'Nothing' when the key is unset. Left to the caller to
-- report: collecting and dedup work without a key.
resolverProvider :: Text -> IO (Maybe KudzuProvider)
resolverProvider url = newKudzuFromEnv url routerKeyEnv

-- | Look a source up by router name.
resolverSource :: KudzuProvider -> Text -> Conspiracy SealedToolChat
resolverSource provider name = do
  sources <- filterToolChats <$> fetchKudzuSources provider
  case Map.lookup name sources of
    -- Same budget and the same reasoning as composition: see
    -- 'AI.Roundup.Compose.withReasoning'.
    Just s -> pure (withReasoning ReasoningLow s)
    Nothing -> throwError . ConspiracyError $
      "no tool-capable source named " <> name <> " (have: "
        <> T.intercalate ", " (Map.keys sources) <> ")"

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

-- | Trim to a length, marking the cut so the model does not read a truncated
-- write-up as a complete one.
clip :: Int -> Text -> Text
clip n t | T.length t <= n = t
         | otherwise = T.take n t <> "… (truncated)"

nonEmpty :: Text -> Maybe Text
nonEmpty t = let s = T.strip t in if T.null s then Nothing else Just s

orElse :: Text -> Text -> Text
orElse a b = fromMaybe b (nonEmpty a)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Lowercase, hyphen-separated, alphanumeric. Slugs reach the URL of a
-- generated page, so whatever the model proposes is normalised rather than
-- trusted.
slugify :: Text -> Text
slugify =
  T.intercalate "-" . filter (not . T.null) . T.split (not . isAlphaNum) . T.toLower . T.strip
