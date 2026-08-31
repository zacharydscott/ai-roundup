{-# LANGUAGE OverloadedStrings #-}

-- | Stage 3: is this news, and if so what does it say?
--
-- One short model call per candidate, fanned out in parallel, returning
-- @'Maybe' 'CandidateSummary'@ — a two-or-three sentence précis, or nothing at
-- all. Nothing means the candidate never reaches another stage.
--
-- __Why this exists.__ Before it, "not worth keeping" was a verdict from the
-- resolver, which meant a listicle cost a full sub-agent conversation with the
-- corpus tools attached before being thrown away. Relevance is the cheapest
-- judgement in the pipeline and it was being made at the most expensive stage.
-- Moving it here inverts that: an irrelevant candidate now costs one short
-- tool-free call, which is what makes it worth subscribing to a feed that is
-- mostly noise and occasionally the only place a story broke.
--
-- __Why 'Maybe' and not a verdict record.__ A record with a boolean and a
-- summary field has to answer "what is the summary of a spiked candidate?",
-- and every available answer is a @Just ""@ — a value that looks present and
-- reads as missing, which is the exact shape "AI.Roundup.Ingest"'s 'nonEmpty'
-- exists to prevent. 'Maybe' has no such state, and it makes the stage's type
-- honest: what leaves here is summaries, so nothing downstream can be handed
-- junk by forgetting to filter.
--
-- The model still needs a way to /say/ "junk", and an omitted field is a
-- weaker signal than a stated one — models tend to fill fields they are shown.
-- So the wire shape keeps an explicit @relevant@ boolean; 'Verdict' collapses
-- to 'Maybe' the moment it is parsed and never escapes this module.
--
-- __The dangerous direction is silent.__ A false keep costs one wasted group.
-- A false spike is invisible: the story never appears and nothing records that
-- it should have. So the prompt is biased towards keeping, every spike is
-- reported with its title and source, and 'TriageOutcome' distinguishes a
-- spike from a failure — a run where a source triaged 0 of 300 should be
-- legible as either correctly noisy or misconfigured, and that is a per-source
-- rate rather than anything a per-item reason string would tell you.

module AI.Roundup.Triage
  ( -- * Output
    CandidateSummary (..)
  , Triaged (..)
    -- * Running triage
  , TriageOutcome (..)
  , TriageProgress
  , triageCandidates
  ) where

import AI.Roundup.ChangeSet (ArticleSpec (..))
import AI.Roundup.Ingest (Candidate (..), candidateSourceIds)

-- Open, for the same reason "AI.Roundup.Compose" does it: this module talks to
-- a source, and @Conspire@ re-exports the monad, the wrappers, the standard
-- configuration and the generation functions as one surface.
import Conspire
import Conspire.Codec.Json
       ( boolCodec, enumCodec, objectCodec, opt, req, stringCodec )

import Control.Concurrent.Async (waitCatch)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

-- | What survives triage: the précis the grouper reads, and the category the
-- expensive stage would otherwise have had to derive for itself.
--
-- The summary is written to be /compared/, not read. Two outlets covering one
-- event should produce summaries that name the same company and the same
-- model, because the next stage decides that they are one story by reading
-- these and nothing else.
data CandidateSummary = CandidateSummary
  { csSummary  :: Text  -- ^ two or three sentences, entity-heavy
  , csCategory :: Text  -- ^ a seeded category slug
  } deriving (Eq, Show)

-- | A candidate that is news, carried together with what it says.
data Triaged = Triaged
  { tgCandidate :: Candidate
  , tgSummary   :: CandidateSummary
  } deriving (Eq, Show)

-- | What happened to one candidate.
--
-- 'Spiked' and 'Failed' are separated deliberately. A spike is a judgement and
-- is expected in bulk; a failure is the model or the router not answering, and
-- a run where those are confused cannot tell a quiet feed from a broken one.
data TriageOutcome
  = Kept CandidateSummary
  | Spiked
  | Failed Text
  deriving (Eq, Show)

-- | Told about each candidate as it finishes: completion count, batch size,
-- the candidate, and what became of it.
--
-- The count is a running total of /completions/, not a position in the input:
-- the fan-out finishes out of order, and pretending otherwise would print a
-- sequence that never happened.
type TriageProgress = Int -> Int -> Candidate -> TriageOutcome -> IO ()

--------------------------------------------------------------------------------
-- The wire shape
--------------------------------------------------------------------------------

-- | The model's answer, before it is collapsed. Never leaves this module.
data Verdict = Verdict
  { vdRelevant :: Bool
  , vdSummary  :: Maybe Text
  , vdCategory :: Maybe Text
  }

verdictFormat :: [Text] -> Format Verdict
verdictFormat cats = Format
  { formatName = "triage"
  , formatCodec = objectCodec "whether this article is AI news, and what it says" $
      Verdict
        <$> req "relevant"
              (boolCodec "true only if this article reports a real event bearing \
                         \on AI as a commercial or institutional story. False if \
                         \it is selling something, if it is opinion or a how-to \
                         \rather than reporting, or if it is not about AI in a \
                         \corporate setting.")
        <*> opt "summary"
              (stringCodec "Two or three sentences, only when relevant is true. \
                           \Name the companies, models, products or rules \
                           \involved and say what actually happened. This is \
                           \compared against other articles' summaries to decide \
                           \which of them cover the same event, so prefer the \
                           \specific noun over the general one.")
        <*> opt "category"
              (enumCodec "which kind of story this is, only when relevant is true" cats)
  , formatDescription = Just "a relevance verdict and, if relevant, a précis"
  }

-- | Collapse the wire shape. An unparseable or out-of-set category falls back
-- to @general_tech@ rather than spiking the candidate: getting the bucket
-- wrong is a thing the corpus can survive and a later stage can correct,
-- whereas dropping real coverage over a mislabelled enum is not.
toSummary :: [Text] -> Verdict -> Maybe CandidateSummary
toSummary cats v
  | not (vdRelevant v) = Nothing
  | otherwise = do
      s <- nonEmpty =<< vdSummary v
      pure (CandidateSummary s category)
  where
    category = fromMaybe fallback $ do
      c <- vdCategory v
      if c `elem` cats then Just c else Nothing
    fallback = if "general_tech" `elem` cats then "general_tech" else headOr "" cats
    headOr d [] = d
    headOr _ (x:_) = x

--------------------------------------------------------------------------------
-- Running it
--------------------------------------------------------------------------------

-- | Triage every candidate, in parallel, keeping only what is news.
--
-- Every candidate is forked at once rather than through a bounded pool here,
-- because conspire's 'Conspire.Manager.Manager' already carries a global
-- in-flight cap enforced by a queue — the throttle exists one layer down and
-- duplicating it here would give the run two limits to reason about. Forked
-- threads are cheap; the requests they make are what queue.
--
-- Order is not preserved and does not matter: the grouper reads the whole set
-- at once, and mechanical dedup has already run.
triageCandidates
  :: StructuredChat s
  => s -> [Text] -> TriageProgress -> [Candidate] -> Conspiracy [Triaged]
triageCandidates src cats report cands = do
  let total = length cands
  counter <- liftIO (newIORef (0 :: Int))
  handles <- mapM (forkConspiracy' . one counter total) cands
  -- 'waitCatch', never 'wait'. A request that times out throws an
  -- 'HttpException' from deep inside http-client — an IO exception, not a
  -- 'ConspiracyError' — so 'catchError' below cannot see it and 'wait' would
  -- rethrow it in this thread and take the whole run down. That is not
  -- hypothetical: a single 600-second timeout killed a 224-candidate run at
  -- number 123, losing every summary already paid for.
  settled <- liftIO (mapM waitCatch handles)
  outcomes <- mapM (uncurry (recover counter total)) (zip cands settled)
  pure [ Triaged c s' | (c, Kept s') <- zip cands outcomes ]
  where
    -- A failed call drops the candidate rather than failing the run: one
    -- unanswered request should cost that article, not the day. It is reported
    -- as a failure rather than a spike so the two never blur in the tally.
    --
    -- One retry, and only ever one. Conspire's 'unwrapJSON' already recovers a
    -- fenced or preamble-wrapped object, so what reaches the retry is a
    -- response that was not JSON at all — usually the fields rendered as YAML
    -- or as bold Markdown. Repeating the identical request would mostly
    -- reproduce the identical answer, so the second attempt says what went
    -- wrong. A third would be paying twice more for the same lesson.
    one counter total cand = attempt (1 :: Int) [userText (candidateBrief cand)]
      where
        attempt n msgs =
          (do (v, _) <- structuredChat src (verdictFormat cats) triagePrompt msgs
              finish counter total cand (maybe Spiked Kept (toSummary cats v)))
            `catchError` \(ConspiracyError msg) ->
              if n > 0
                then attempt (n - 1) (msgs ++ [userText (reformat msg)])
                else finish counter total cand (Failed msg)

    reformat msg = T.unlines
      [ "That could not be parsed: " <> firstLine msg
      , ""
      , "Answer again with a JSON object and nothing else. No prose before or"
      , "after it, no Markdown fence, no bold. It must start with { and end"
      , "with }."
      ]

    -- A thread killed by an IO exception never reached 'finish', so its line
    -- is printed here instead. Out of order with the rest, which is correct:
    -- it finished when it died, not now.
    recover counter total cand settled = case settled of
      Right (Right outcome)             -> pure outcome
      Right (Left (ConspiracyError msg)) -> finish counter total cand (Failed msg)
      Left err                          -> finish counter total cand
                                             (Failed (firstLine (T.pack (show err))))

    finish counter total cand outcome = do
      n <- liftIO $ atomicModifyIORef' counter (\k -> (k + 1, k + 1))
      liftIO (report n total cand outcome)
      pure outcome

-- | An 'HttpException' renders as a twenty-line request dump with a call stack.
-- One line of it is the diagnosis; the rest is noise in a per-candidate log.
firstLine :: Text -> Text
firstLine = T.strip . T.takeWhile (/= '{') . T.replace "\n" " "

--------------------------------------------------------------------------------
-- What the model is shown
--------------------------------------------------------------------------------

-- | One candidate, as labelled lines. Nothing parses this, so JSON would only
-- spend tokens on syntax.
--
-- The other sources carrying the same article are named but their titles are
-- not repeated: how many feeds picked something up is a salience signal worth
-- having, and at this stage it is the only one available.
candidateBrief :: Candidate -> Text
candidateBrief cand = T.unlines $
  [ "title:    " <> asTitle art
  , "outlet:   " <> fromMaybe "(unknown)" (asOutlet art)
  , "source:   " <> asSourceId art
  , "url:      " <> asUrlCanonical art
  , "date:     " <> fromMaybe "(none given)" (asPublishedAt art)
  , "excerpt:  " <> fromMaybe "(none)" (asExcerpt art)
  ]
  ++ [ "also carried by: " <> T.intercalate ", " others | not (null others) ]
  where
    art = cndArticle cand
    others = drop 1 (candidateSourceIds cand)

triagePrompt :: SystemPrompt
triagePrompt = T.unlines
  [ "You are the first filter on a stream of articles pulled from RSS feeds and"
  , "web searches. Most of them do not belong in this corpus."
  , ""
  , "The corpus tracks AI as a commercial and institutional story: what the labs"
  , "and the companies around them ship, buy, charge for, get sued over and get"
  , "regulated by. A reader of it is trying to follow the industry, not to learn"
  , "how to use the tools or to think about what it all means."
  , ""
  , "Set relevant to true when the article reports something that actually"
  , "happened, and it bears on that story: a model or product release, a"
  , "capability or benchmark result, a safety or security finding, a regulation"
  , "or enforcement action, a court ruling, a funding round, an acquisition, a"
  , "partnership, a pricing or quota change, an infrastructure or supply deal,"
  , "hiring or layoffs at scale."
  , ""
  , "Set relevant to false in each of these three cases."
  , ""
  , "  1. It is selling something. Advertisements, marketing copy, sponsored"
  , "     posts, launch announcements for a tool by the person who built it,"
  , "     'Show HN' style self-promotion, vendor case studies whose point is"
  , "     that the vendor's product worked. If the article exists to make you"
  , "     want a product, it is out — including when the product is free."
  , ""
  , "  2. It is commentary or instruction rather than reporting. Opinion,"
  , "     analysis and think-pieces with no new event behind them; how-tos,"
  , "     tutorials, guides, 'lessons learned' posts; listicles and 'best tools'"
  , "     roundups; link-blog posts that are a quote or a pointer with no"
  , "     reporting of their own."
  , ""
  , "  3. It is not about AI in a corporate setting. Consumer gadget and gaming"
  , "     news; academic or scientific results with no bearing on what the"
  , "     industry is shipping; AI in education, religion, art or society"
  , "     considered as a cultural phenomenon; personal projects; corporate"
  , "     housekeeping such as appointments, award listings and scheduled"
  , "     investor events; routine point releases of small tools."
  , ""
  , "Apply those three first. They are not close calls and they are most of what"
  , "you will see."
  , ""
  , "If an article survives all three and you are still unsure whether it is"
  , "substantial enough, keep it. At that point the asymmetry favours keeping: a"
  , "borderline article costs a later stage a little work, while one you wrongly"
  , "reject is gone silently and nobody finds out. But that is a tie-breaker for"
  , "genuine borderline cases, not licence to keep something the three rules"
  , "above already excluded."
  , ""
  , "Always set category when relevant is true. Pick the most specific one that"
  , "fits; general_tech is the fallback for things none of the others describe,"
  , "not the default:"
  , ""
  , "  model         a model or a capability of one: releases, versions,"
  , "                benchmarks, evaluations, research results, safety findings"
  , "  regulation    law, courts, enforcement, government policy, standards"
  , "  business      funding, acquisitions, partnerships, earnings, hiring at"
  , "                scale, lawsuits between companies, infrastructure deals"
  , "  pricing       what something costs: price changes, tiers, quotas, cost"
  , "                or efficiency claims about serving a model"
  , "  general_tech  real AI news that is none of the above"
  , ""
  , "The summary is not for a reader. It is compared against the summaries of"
  , "every other article in this batch to work out which of them are covering"
  , "the same event. Name the company, the model, the product, the regulation."
  , "Two articles about one release should produce two summaries that obviously"
  , "describe one release."
  ]

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

-- | Blank and absent are the same thing, for the reason "AI.Roundup.Ingest"
-- gives: a @Just \"\"@ downstream looks present and reads as missing.
nonEmpty :: Text -> Maybe Text
nonEmpty t = let s = T.strip t in if T.null s then Nothing else Just s
