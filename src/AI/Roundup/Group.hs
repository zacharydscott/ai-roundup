{-# LANGUAGE OverloadedStrings #-}

-- | Stage 4: which of these articles are about the same event?
--
-- One model call over every surviving summary at once, returning a partition.
-- That "at once" is the whole point, and it is what replaced the previous
-- design's most intricate machinery.
--
-- __What this deletes.__ Resolution used to run one candidate at a time, so
-- two articles about the same brand-new story could each conclude "no story
-- covers this" and mint two. Guarding against that took a @RunLedger@ threaded
-- through the fold, a pending-stories list injected into every search result,
-- a rule refusing a @new@ verdict until a search had run, and a repeat of the
-- pending list in each brief — four independent mechanisms, all of them
-- compensating for the fact that a candidate could not see its neighbours.
-- A grouper that sees the whole batch makes that failure unrepresentable:
-- there is exactly one group per event because the partition says so, and no
-- amount of model misbehaviour downstream can split one into two.
--
-- __Grouping is not resolution.__ This stage only decides which candidates
-- belong together. Whether the resulting group is new to the corpus or more
-- coverage of a story already in it is "AI.Roundup.Resolve"'s job, and needs
-- the database. Keeping them apart is what lets resolution run in parallel:
-- groups are independent once the partition is fixed.
--
-- __Nothing may be dropped here.__ A candidate the model forgets to mention,
-- names twice, or names with an out-of-range index still ends up in exactly
-- one group — its own, if nothing else claimed it. Losing coverage to a
-- malformed partition would be silent, and silence is the failure mode this
-- pipeline spends the most effort avoiding.

module AI.Roundup.Group
  ( CandidateGroup (..)
  , groupTriaged
  , groupArticles
  , groupPrimaryArticles
  ) where

import AI.Roundup.ChangeSet (ArticleSpec (..))
import AI.Roundup.Ingest (Candidate (..))
import AI.Roundup.Triage (CandidateSummary (..), Triaged (..))

import Conspire
import Conspire.Codec.Json
       ( arrayCodec, integerCodec, objectCodec, req, stringCodec )

import Control.Monad.Except (catchError)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

-- | One event's worth of coverage.
--
-- 'cgTitle' is the grouper's proposal for the story as a whole rather than any
-- one article's headline, which is the distinction that matters: three outlets
-- covering a release write three headlines about their own angle, and the
-- story is the release.
data CandidateGroup = CandidateGroup
  { cgTitle    :: Text
  , cgCategory :: Text       -- ^ the plurality of its members' triage categories
  , cgMembers  :: [Triaged]  -- ^ oldest published first, never empty
  } deriving (Eq, Show)

-- | Every article a group carries, in member order: the winning copy of each
-- member, and the mechanically-collapsed copies from other feeds.
--
-- Those copies share a canonical URL with the copy that won, so at most one of
-- them can ever become a row — @article.url_canonical@ is @UNIQUE@. They are
-- here because the earliest-coverage anchor is a @min@ over all of them, and
-- an anchor that ignored a syndicating feed's earlier timestamp would date the
-- story wrongly.
groupArticles :: CandidateGroup -> [ArticleSpec]
groupArticles grp =
  [ a | t <- cgMembers grp
      , a <- cndArticle (tgCandidate t) : cndDuplicates (tgCandidate t) ]

-- | The articles that become rows: one per member, the copy that won dedup.
--
-- Deliberately not 'groupArticles'. The collapsed copies share a canonical URL
-- with the winner, so passing them on would hand the writer the same URL
-- several times and hand the applier rows it is obliged to discard. They earn
-- their place in the anchor calculation and nowhere else.
groupPrimaryArticles :: CandidateGroup -> [ArticleSpec]
groupPrimaryArticles grp = map (cndArticle . tgCandidate) (cgMembers grp)

--------------------------------------------------------------------------------
-- The wire shape
--------------------------------------------------------------------------------

data GroupSpec = GroupSpec
  { gsTitle   :: Text
  , gsMembers :: [Int]
  }

newtype Grouping = Grouping { grGroups :: [GroupSpec] }

groupingFormat :: Format Grouping
groupingFormat = Format
  { formatName = "grouping"
  , formatCodec = objectCodec "a partition of the numbered articles into stories" $
      Grouping
        <$> req "stories" (arrayCodec "one entry per distinct event" groupSpecCodec)
  , formatDescription = Just "which of the numbered articles cover the same event"
  }
  where
    groupSpecCodec = objectCodec "one event and the articles covering it" $
      GroupSpec
        <$> req "title"
              (stringCodec "A headline for the event itself, not for any one \
                           \article about it. Twelve words at the outside.")
        <*> req "articles"
              (arrayCodec "the numbers of every article covering this event"
                          (integerCodec "an article number from the list"))

--------------------------------------------------------------------------------
-- Running it
--------------------------------------------------------------------------------

-- | Partition the triaged candidates into stories.
--
-- A single call: the summaries are short by construction and the whole batch
-- has to be visible at once for the partition to mean anything. An empty input
-- short-circuits rather than asking a model to partition nothing.
groupTriaged :: StructuredChat s => s -> [Triaged] -> Conspiracy [CandidateGroup]
groupTriaged _ [] = pure []
groupTriaged src triaged =
  (do (grouping, _) <- structuredChat src groupingFormat groupingPrompt
                         [ userText (batchBrief triaged) ]
      pure (assemble triaged (grGroups grouping)))
    -- A failed partition degrades to no partition: every candidate becomes its
    -- own story. That is the same repair 'assemble' already applies to an
    -- article the model forgot, taken to its limit, and it is the only
    -- degradation that keeps the run's promise — nothing collected is lost.
    -- The cost is real but bounded and visible: some stories arrive split that
    -- should have been one, which a later run's resolver can still merge,
    -- whereas a dropped day cannot be recovered at all.
    `catchError` \(ConspiracyError _) -> pure (assemble triaged [])

-- | Turn the model's index lists into groups, and make them total.
--
-- Three defences, in order: an index outside the batch is discarded, an index
-- claimed twice belongs to whichever group claimed it first, and anything left
-- unclaimed becomes a group of its own. The last is the one that matters — a
-- model that returns a short list, or an empty one, costs extra resolver calls
-- and not a single article.
assemble :: [Triaged] -> [GroupSpec] -> [CandidateGroup]
assemble triaged specs = orderGroups (named ++ orphans)
  where
    indexed = M.fromList (zip [0 :: Int ..] triaged)
    total   = length triaged

    (named, claimed) = foldl step ([], Set.empty) specs
      where
        step (acc, taken) spec =
          let picks = [ i | i <- gsMembers spec
                          , i >= 0 && i < total
                          , not (Set.member i taken) ]
          in case mapMaybe (`M.lookup` indexed) picks of
               []      -> (acc, taken)
               members -> ( acc ++ build (gsTitle spec) members
                          , foldr Set.insert taken picks )

    orphans = concat
      [ build (asTitle (cndArticle (tgCandidate t))) [t]
      | (i, t) <- zip [0 ..] triaged, not (Set.member i claimed) ]

-- | Build a group, fixing its member order and its category.
--
-- The category is the plurality of the members' triage verdicts, ties broken
-- by the earliest-published member — one outlet filing a release under
-- @business@ should not outvote two that called it @model@.
--
-- Returns a list so that it is total: a group with no members is not a group,
-- and every caller here already has a non-empty list, so the empty case is
-- unreachable rather than handled.
build :: Text -> [Triaged] -> [CandidateGroup]
build title members = case sortOn memberKey members of
  [] -> []
  ordered@(first : _) ->
    [ CandidateGroup
        { cgTitle    = fromMaybe (asTitle (cndArticle (tgCandidate first)))
                                 (nonEmpty title)
        , cgCategory = plurality ordered
        , cgMembers  = ordered
        } ]

plurality :: [Triaged] -> Text
plurality members = case [ c | c <- cats, M.lookup c tally == Just best ] of
  (c : _) -> c
  []      -> "general_tech"
  where
    cats  = map (csCategory . tgSummary) members
    tally = M.fromListWith (+) [ (c, 1 :: Int) | c <- cats ]
    best  = if M.null tally then 0 else maximum (M.elems tally)

-- | Undated last, for the reason "AI.Roundup.Ingest" gives: a dateless search
-- hit must not be allowed to anchor a story earlier than the coverage that
-- actually broke it.
memberKey :: Triaged -> (Bool, Text, Text)
memberKey t = (null published, headOr "" published, asUrlCanonical art)
  where
    art = cndArticle (tgCandidate t)
    published = maybe [] pure (asPublishedAt art)
    headOr d [] = d
    headOr _ (x:_) = x

-- | Groups in the order their events happened, so a day's stories reach the
-- writer oldest first.
orderGroups :: [CandidateGroup] -> [CandidateGroup]
orderGroups = sortOn key
  where
    key grp = case mapMaybe asPublishedAt (groupArticles grp) of
      []   -> (True, "", cgTitle grp)
      pubs -> (False, minimum pubs, cgTitle grp)

--------------------------------------------------------------------------------
-- What the model is shown
--------------------------------------------------------------------------------

-- | Every summary, numbered. The numbers are the whole interface: the model
-- answers in them, so they are the one thing that must be unambiguous.
batchBrief :: [Triaged] -> Text
batchBrief triaged = T.unlines
  [ T.unlines
      [ "[" <> T.pack (show i) <> "] " <> asTitle (cndArticle (tgCandidate t))
      , "    " <> csSummary (tgSummary t)
      , "    (" <> csCategory (tgSummary t) <> ", "
          <> fromMaybe "(no date)" (asPublishedAt (cndArticle (tgCandidate t))) <> ")"
      ]
  | (i, t) <- zip [0 :: Int ..] triaged
  ]

groupingPrompt :: SystemPrompt
groupingPrompt = T.unlines
  [ "You are given every AI news article collected today, numbered, each with a"
  , "short summary. Partition them into events."
  , ""
  , "Two articles belong to the same event when they describe the same thing"
  , "happening: one model release, one regulation, one acquisition, one"
  , "incident. Different outlets covering one announcement is one event, however"
  , "differently they headline it."
  , ""
  , "Two articles about the same company, or the same model family, are NOT the"
  , "same event unless they are about the same occurrence. A release and a"
  , "review of that release are two events. Two versions of one model are two"
  , "events."
  , ""
  , "Every article number must appear in exactly one story. An article covering"
  , "an event nothing else covers is a story on its own — most of them will be."
  , "Do not leave any number out, and do not use any number twice."
  ]

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

nonEmpty :: Text -> Maybe Text
nonEmpty t = let s = T.strip t in if T.null s then Nothing else Just s
