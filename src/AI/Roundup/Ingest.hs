{-# LANGUAGE OverloadedStrings #-}

-- | Stages 1 and 2: what the sources said, mechanically deduplicated, before
-- any model has seen a token of it.
--
-- 'Candidate' is the seam between ingest and the resolver. It wraps an
-- 'ArticleSpec' rather than repeating its fields because that record is
-- already the exact shape of an @article@ row, and a parallel set of fields
-- here would be one rename away from drifting out of sync with it. The
-- resolver's input is @(article, duplicates)@, so the adapter over this type
-- is a rename and nothing more.
--
-- Two levels of "the same", and the difference is the whole design:
--
--   * __Identity__ — same canonical URL, or same guid. This is one article
--     that two sources carried. It collapses: one 'Candidate', the extra
--     copies kept in 'cndDuplicates' (never discarded: which sources carried
--     a piece is provenance, and how many of them did is a salience signal
--     the resolver can use).
--   * __Similarity__ — same or near-same title at different URLs. This is two
--     articles about one story. It must not collapse — that would throw away
--     a real piece of coverage — so it only /groups/: the candidates share a
--     'cndGroupKey' and the resolver sees them together, producing one story
--     with two articles.
--
-- Both run before the LLM, so both are cheap and boring on purpose: string
-- normalisation, a map, and a token-overlap test. Anything needing judgement
-- is the resolver's job.
--
-- Timestamps are ISO-8601 UTC 'Text' from the moment a candidate is built,
-- for the reason "AI.Roundup.Store" gives: in that format lexicographic order
-- is chronological order, so ordering candidates and comparing them against
-- stored dates are the same plain string comparison.

module AI.Roundup.Ingest
  ( -- * The seam
    Candidate (..)
  , candidateSourceIds
    -- * Stage 1: collect
  , IngestFailure (..)
  , collectCandidates
  , feedCandidate
  , tavilyCandidate
    -- * Stage 2: mechanical dedup
  , dedupCandidates
  , candidateGroups
    -- * Normalisation
  , canonicalUrl
  , outletFromUrl
  , excerptFrom
  , isoUTC
  ) where

import AI.Roundup.ChangeSet (ArticleSpec (..))
import AI.Roundup.Config (FeedSource (..), SourcesConfig (..), TavilySource (..))
import AI.Roundup.Ingest.Feed (FeedItem (..), fetchFeed)
import AI.Roundup.Ingest.Tavily (TavilyResult (..), tavilySearch)
import Conspire.MCP (Client)
import Conspire.Monad (Conspiracy, ConspiracyError (..))
import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit, isSpace)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Client (Manager)

--------------------------------------------------------------------------------
-- The candidate
--------------------------------------------------------------------------------

-- | One article this run might want, plus what dedup learned about it.
data Candidate = Candidate
  { cndArticle :: ArticleSpec
    -- ^ The copy that wins: the first source in @sources.yaml@ order to carry
    -- this URL, with any gaps (outlet, excerpt, guid, date) filled in from the
    -- copies that collapsed into it. Vendor blogs listed above aggregators is
    -- therefore not cosmetic — it decides whose headline is stored.
  , cndDuplicates :: [ArticleSpec]
    -- ^ The other sources' copies of the same article, in collect order. They
    -- share a canonical URL with 'cndArticle', so at most one of them can ever
    -- become a row (@article.url_canonical@ is @UNIQUE@); they are carried for
    -- provenance and as context for the resolver, not to be inserted.
  , cndGroupKey :: Text
    -- ^ Candidates sharing this key are believed to be one story. Before
    -- 'dedupCandidates' every candidate is its own group and this is just its
    -- canonical URL, so the field is meaningful at every stage rather than
    -- being a hole waiting to be filled.
  } deriving (Eq, Show)

-- | Every source that carried this article, winner first. Both a debugging
-- aid ("why is this here?") and the count the resolver can read as interest.
candidateSourceIds :: Candidate -> [Text]
candidateSourceIds c = map asSourceId (cndArticle c : cndDuplicates c)

-- | A source that failed. Collected rather than thrown: one dead feed in a
-- list of four should cost that feed, not the run, and a run that silently
-- ingested three quarters of the world is exactly the failure that later
-- reads as a quiet news day.
data IngestFailure = IngestFailure
  { ifSourceId :: Text
  , ifReason   :: Text
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Stage 1: collect
--------------------------------------------------------------------------------

-- | Fetch every source in the config.
--
-- In 'Conspiracy' because Tavily is reached over conspire's MCP client, which
-- lives there; the feed 'Manager' is passed separately because conspire's
-- 'Conspire.Manager.Manager' is a different, wrapped thing and @fetchFeed@
-- wants the @http-client@ one.
--
-- 'Nothing' for the client means no @TAVILY_API_KEY@: the searches are then
-- reported as failures rather than skipped silently, because a roundup built
-- from feeds alone is a different roundup and the operator should know.
--
-- No deduplication happens here — that is 'dedupCandidates', kept separate so
-- the raw haul can be dumped and eyeballed.
collectCandidates
  :: Manager -> Maybe Client -> SourcesConfig
  -> Conspiracy ([Candidate], [IngestFailure])
collectCandidates manager mClient cfg = do
  now <- liftIO getCurrentTime
  feeds  <- liftIO $ mapM (collectFeed manager now) (cfgFeeds cfg)
  tavily <- mapM (collectSearch mClient now) (cfgTavily cfg)
  let (failures, hauls) = partitionEithers (feeds <> tavily)
  pure (filter (recentEnough now (cfgMaxAgeDays cfg)) (concat hauls), failures)

-- | Apply the configured window.
--
-- Undated candidates survive it. The window exists to keep an archive feed
-- from being read as today's news, and "no date" is not evidence of age —
-- dropping those would silently discard most of what search returns.
recentEnough :: UTCTime -> Maybe Int -> Candidate -> Bool
recentEnough now mDays c = case (mDays, asPublishedAt (cndArticle c)) of
  (Nothing, _)             -> True
  (_, Nothing)             -> True
  (Just days, Just published) ->
    published >= isoUTC (addUTCTime (negate (fromIntegral days * 86400)) now)

collectFeed :: Manager -> UTCTime -> FeedSource -> IO (Either IngestFailure [Candidate])
collectFeed manager now src = do
  -- 'fetchFeed' returns Left for an HTTP or parse failure but throws for a
  -- connection failure, by its own documented policy of leaving that decision
  -- to the caller. This is the caller, and the decision is: log it and carry on.
  fetched <- try (fetchFeed manager (fsUrl src))
  pure $ case fetched of
    Left err            -> failed (T.pack (show (err :: SomeException)))
    Right (Left reason) -> failed reason
    Right (Right items) -> Right (mapMaybe (feedCandidate now src) items)
  where failed = Left . IngestFailure (fsId src)

collectSearch
  :: Maybe Client -> UTCTime -> TavilySource
  -> Conspiracy (Either IngestFailure [Candidate])
collectSearch Nothing _ src =
  pure (Left (IngestFailure (tsId src) "TAVILY_API_KEY is not set; search skipped"))
collectSearch (Just client) now src =
  (Right . mapMaybe (tavilyCandidate now src) <$> tavilySearch client src)
    `catchError` \(ConspiracyError reason) ->
      pure (Left (IngestFailure (tsId src) reason))

-- | A feed item as a candidate, or 'Nothing' for an item with no link.
--
-- Dropping the linkless is not a judgement call: @article.url_canonical@ is
-- @NOT NULL UNIQUE@ and is the identity of everything downstream, so an item
-- we cannot address is not an article we can store, group, or ever show a
-- reader.
feedCandidate :: UTCTime -> FeedSource -> FeedItem -> Maybe Candidate
feedCandidate now src item = do
  link <- itemLink item
  let canonical = canonicalUrl link
  if T.null canonical then Nothing else Just $ candidate ArticleSpec
    { asUrlCanonical = canonical
    , asUrlOriginal  = link
    , asTitle        = T.strip (itemTitle item)
    , asOutlet       = fsOutlet src <|> outletFromUrl canonical
    , asSourceId     = fsId src
    , asGuid         = nonEmpty (itemGuid item)
    , asPublishedAt  = isoUTC <$> itemDate item
    , asFetchedAt    = isoUTC now
    , asExcerpt      = excerptFrom =<< itemSummary item
    }

-- | A search hit as a candidate.
--
-- No guid: a search result has no feed identity, and inventing one from the
-- URL would only duplicate the canonical-URL key that already covers it.
tavilyCandidate :: UTCTime -> TavilySource -> TavilyResult -> Maybe Candidate
tavilyCandidate now src result
  | T.null canonical = Nothing
  | otherwise = Just $ candidate ArticleSpec
      { asUrlCanonical = canonical
      , asUrlOriginal  = trUrl result
      , asTitle        = T.strip (trTitle result)
      , asOutlet       = outletFromUrl canonical
      , asSourceId     = tsId src
      , asGuid         = Nothing
      , asPublishedAt  = isoUTC <$> (parseSearchDate =<< trPublishedDate result)
      , asFetchedAt    = isoUTC now
      , asExcerpt      = excerptFrom (trContent result)
      }
  where canonical = canonicalUrl (trUrl result)

-- | A fresh candidate: its own group, nothing collapsed into it yet.
candidate :: ArticleSpec -> Candidate
candidate spec = Candidate spec [] (asUrlCanonical spec)

-- | Search dates are re-parsed and re-rendered rather than passed through, so
-- that every 'asPublishedAt' in the run is in one format and string ordering
-- stays chronological ordering.
parseSearchDate :: Text -> Maybe UTCTime
parseSearchDate raw = firstJust [ attempt f | f <- formats ]
  where
    attempt f = parseTimeM True defaultTimeLocale f (T.unpack (T.strip raw))
    formats =
      [ "%Y-%m-%dT%H:%M:%S%Z", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S"
      , "%Y-%m-%d", "%a, %d %b %Y %H:%M:%S %Z" ]
    firstJust xs = case [ x | Just x <- xs ] of
      (x:_) -> Just x
      []    -> Nothing

--------------------------------------------------------------------------------
-- Canonical URLs
--------------------------------------------------------------------------------

-- | The form of a URL used for identity: no fragment, no tracking parameters,
-- no trailing slash, host lower-cased and de-@www@-ed.
--
-- Hand-rolled rather than via a URI library because the rules that matter are
-- a handful of string operations and every one of them is a policy decision
-- ("is @?ref=@ tracking or content?") that a general parser would not make for
-- us anyway. Anything it does not understand it leaves alone: an unparseable
-- URL that stays whole still dedups against itself.
--
-- The scheme is kept as given. Rewriting @http@ to @https@ would merge a few
-- more duplicates and silently rewrite the address of the handful of sites
-- that genuinely have no TLS.
canonicalUrl :: Text -> Text
canonicalUrl raw = scheme <> normaliseHost authority
                          <> normalisePath rawPath
                          <> renderQuery (filter (not . isTracking) (queryParams rawQuery))
  where
    trimmed = T.strip raw
    (scheme, afterScheme) = case T.breakOn "://" trimmed of
      (s, r) | not (T.null r) -> (T.toLower s <> "://", T.drop 3 r)
      _                       -> ("", trimmed)
    beforeFragment = T.takeWhile (/= '#') afterScheme
    (authority, rest) = T.break (\c -> c == '/' || c == '?') beforeFragment
    (rawPath, rawQuery) = T.break (== '?') rest

normaliseHost :: Text -> Text
normaliseHost = dropWww . dropDefaultPort . T.toLower . dropUserInfo
  where
    dropUserInfo a = case T.breakOnEnd "@" a of
      ("", _)   -> a
      (_, host) -> host
    dropDefaultPort h
      | ":80"  `T.isSuffixOf` h = T.dropEnd 3 h
      | ":443" `T.isSuffixOf` h = T.dropEnd 4 h
      | otherwise               = h
    dropWww h = fromMaybe h (T.stripPrefix "www." h)

-- | @\/a\/b\/@ and @\/a\/b@ are the same page everywhere it matters, and feeds
-- disagree about which one to publish. The empty path is left empty rather
-- than becoming @\/@ so that @example.com@ and @example.com\/@ agree too.
normalisePath :: Text -> Text
normalisePath = T.dropWhileEnd (== '/')

queryParams :: Text -> [Text]
queryParams = filter (not . T.null) . T.splitOn "&" . T.drop 1

-- | Parameter order is preserved rather than sorted: the canonical URL is
-- stored and linked, not just hashed, and a reordered query is a URL some
-- servers will not answer.
renderQuery :: [Text] -> Text
renderQuery [] = ""
renderQuery ps = "?" <> T.intercalate "&" ps

-- | Campaign and referrer parameters, which change per link to the same page
-- and are the single biggest source of false distinct URLs.
--
-- The list is conservative on purpose. @ref@, @src@ and @source@ are included
-- because in feed links they are overwhelmingly attribution; the cost of being
-- wrong is two distinct articles merging into one, so anything less clear-cut
-- stays out.
isTracking :: Text -> Bool
isTracking param = "utm_" `T.isPrefixOf` name || name `elem` tracking
  where
    name = T.toLower (T.takeWhile (/= '=') param)
    tracking =
      [ "fbclid", "gclid", "gbraid", "wbraid", "msclkid", "yclid", "igshid"
      , "mc_cid", "mc_eid", "_hsenc", "_hsmi", "vero_id"
      , "ref", "ref_src", "referrer", "source", "src", "cmpid", "ncid", "spm"
      , "at_medium", "at_campaign", "guccounter", "guce_referrer"
      , "guce_referrer_sig", "__twitter_impression" ]

-- | The publisher, guessed from the host, for sources that carry other
-- people's writing. A bare host (@techcrunch.com@) rather than a pretty name:
-- it is honest about being derived, and the composer can render it nicely.
outletFromUrl :: Text -> Maybe Text
outletFromUrl url = nonEmpty host
  where
    host = normaliseHost
         . T.takeWhile (\c -> c /= '/' && c /= '?')
         . snd
         $ T.breakOnEnd "://" url

--------------------------------------------------------------------------------
-- Excerpts
--------------------------------------------------------------------------------

-- | Feed summaries arrive as HTML of wildly varying length. The resolver and
-- the composer pay for every character of it, so it is flattened to text and
-- cut here, once, rather than being carried into the prompts whole.
excerptFrom :: Text -> Maybe Text
excerptFrom = nonEmpty . truncateAt 400 . collapse . unescape . stripTags

stripTags :: Text -> Text
stripTags t = case T.breakOn "<" t of
  (before, rest)
    | T.null rest -> before
    | otherwise   -> before <> " " <> stripTags (T.drop 1 (T.dropWhile (/= '>') rest))

-- | Only the entities that actually show up in feed summaries. A full entity
-- table would be a dependency for a cosmetic gain on text nobody reads raw.
unescape :: Text -> Text
unescape t = foldl' (\acc (from, to) -> T.replace from to acc) t
  [ ("&amp;", "&"), ("&lt;", "<"), ("&gt;", ">"), ("&quot;", "\"")
  , ("&#39;", "'"), ("&#x27;", "'"), ("&apos;", "'"), ("&nbsp;", " ")
  , ("&hellip;", "…"), ("&rsquo;", "’"), ("&lsquo;", "‘")
  , ("&ldquo;", "\""), ("&rdquo;", "\""), ("&mdash;", "—"), ("&ndash;", "–") ]

collapse :: Text -> Text
collapse = T.unwords . T.words

truncateAt :: Int -> Text -> Text
truncateAt n t
  | T.length t <= n = t
  | T.null atWord   = T.take n t <> "…"
  | otherwise       = atWord <> "…"
  where atWord = T.stripEnd (T.dropWhileEnd (not . isSpace) (T.take n t))

--------------------------------------------------------------------------------
-- Stage 2: mechanical dedup
--------------------------------------------------------------------------------

-- | Collapse duplicates, group near-duplicates, and put the result in the
-- order the resolver wants to see it.
dedupCandidates :: [Candidate] -> [Candidate]
dedupCandidates = orderForResolution . groupByTitle . collapseByIdentity

-- | The groups 'dedupCandidates' found, as lists. Its output keeps group
-- members adjacent, so this is a scan rather than a second sort.
candidateGroups :: [Candidate] -> [[Candidate]]
candidateGroups = groupBy ((==) `on` cndGroupKey)

--------------------------------------------------------------------------------
-- Identity: canonical URL, then guid
--------------------------------------------------------------------------------

-- | Fold the list into slots, keyed by every identity a candidate has. First
-- one to claim a key keeps it, so the winner is the earliest source in
-- @sources.yaml@ order, deterministically.
--
-- A candidate matching two /different/ existing slots merges into the first
-- and leaves the second alone. That is a transitive merge this deliberately
-- does not do: it needs union-find over identities to fix, it happens only
-- when a feed republishes under a new URL that a third feed also carries, and
-- the title pass below groups the survivors anyway.
collapseByIdentity :: [Candidate] -> [Candidate]
collapseByIdentity = elems . foldl' place (M.empty, M.empty, 0 :: Int)
  where
    elems (_, slots, _) = M.elems slots   -- Int keys: ascending is first-seen

    place (index, slots, next) c = case mapMaybe (`M.lookup` index) keys of
      (slot:_) -> (claim slot, M.adjust (`absorb` c) slot slots, next)
      []       -> (claim next, M.insert next c slots, next + 1)
      where
        keys    = identityKeys c
        claim s = foldl' (\m k -> M.insertWith (\_ old -> old) k s m) index keys

identityKeys :: Candidate -> [Text]
identityKeys c = ("u:" <> asUrlCanonical spec) : guidKey
  where
    spec = cndArticle c
    -- A guid is only unique within its own feed unless it is self-evidently
    -- global. "1234" from two feeds is two different articles; a URL or a
    -- tag: URI is one. Scoping the doubtful case to its source keeps guid
    -- doing what guid is for — catching a feed republishing the same item at
    -- a new URL — without inventing cross-feed matches out of small integers.
    guidKey = case nonEmpty =<< asGuid spec of
      Nothing -> []
      Just g
        | "://" `T.isInfixOf` g || T.length g >= 20 -> ["g:" <> g]
        | otherwise -> ["g:" <> asSourceId spec <> "\t" <> g]

-- | Fold a later copy into the winner: keep the winner's own fields, take
-- anything it is missing, and keep the /earliest/ publication date seen,
-- because the date is the story's anchor and a syndicating feed that stamps
-- its own re-publication time must not be allowed to move it forward.
absorb :: Candidate -> Candidate -> Candidate
absorb keep extra = keep
  { cndArticle = (cndArticle keep)
      { asOutlet      = asOutlet kept      <|> asOutlet other
      , asGuid        = asGuid kept        <|> asGuid other
      , asExcerpt     = longer (asExcerpt kept) (asExcerpt other)
      , asPublishedAt = earlier (asPublishedAt kept) (asPublishedAt other)
      }
  , cndDuplicates = cndDuplicates keep <> (other : cndDuplicates extra)
  }
  where
    kept  = cndArticle keep
    other = cndArticle extra
    longer a b
      | maybe 0 T.length a >= maybe 0 T.length b = a <|> b
      | otherwise                                = b
    earlier (Just a) (Just b) = Just (min a b)   -- ISO-8601: min is earliest
    earlier a b               = a <|> b

--------------------------------------------------------------------------------
-- Similarity: exact title, then near title
--------------------------------------------------------------------------------

-- | Assign group keys by title. Quadratic in the number of candidates, which
-- for a few hundred a day is nothing, and the alternative (blocking, indexing)
-- would be machinery in service of a cost nobody is paying.
groupByTitle :: [Candidate] -> [Candidate]
groupByTitle cs =
  [ c { cndGroupKey = keyOf i } | (i, c) <- indexed ]
  where
    indexed = zip [0 :: Int ..] cs
    prints  = M.fromList [ (i, fingerprint (asTitle (cndArticle c))) | (i, c) <- indexed ]

    merged = foldl' union (M.fromList [ (i, i) | (i, _) <- indexed ])
      [ (i, j)
      | (i, _) <- indexed, (j, _) <- indexed, i < j
      , sameStory (prints M.! i) (prints M.! j)
      ]

    -- The lowest canonical URL in the class names the group: stable whatever
    -- order the sources were fetched in, and readable in a dump.
    keyByRoot = M.fromListWith min
      [ (root merged i, asUrlCanonical (cndArticle c)) | (i, c) <- indexed ]
    keyOf i = keyByRoot M.! root merged i

    root parent i = let p = parent M.! i in if p == i then i else root parent p
    union parent (i, j) =
      let ri = root parent i
          rj = root parent j
      in if ri == rj then parent else M.insert (max ri rj) (min ri rj) parent

-- | A title reduced to what can be compared: the normalised string for the
-- exact test, and its significant tokens for the near test.
fingerprint :: Text -> (Text, Set.Set Text)
fingerprint title = (normalised, Set.fromList (filter significant (T.words normalised)))
  where
    normalised = collapse (T.map keep (T.toLower title))
    keep c
      | c `elem` ("0123456789abcdefghijklmnopqrstuvwxyz" :: String) = c
      | otherwise = ' '
    -- Short tokens are noise except when they are digits, and in this corpus
    -- the digits are the story: "GPT-5.2-Codex" and "GPT-5.3-Codex" share
    -- every word and are two different releases.
    significant w = (T.length w > 2 || T.any isDigit w) && w `notElem` stopwords
    stopwords =
      [ "the", "and", "for", "its", "with", "from", "that", "this", "has"
      , "are", "was", "will", "new", "now", "says", "said", "after", "over"
      , "into", "how", "why", "you", "your", "not", "but", "our", "out" ]

-- | Two candidates are one story if their titles match once normalised, or if
-- their significant words overlap heavily.
--
-- The threshold is high and the minimum token count is three because the
-- failure mode is asymmetric: grouping two unrelated stories corrupts the
-- corpus and needs a human to unpick, while missing a grouping costs one
-- resolver call that reaches the same conclusion. "OpenAI launches GPT-5" and
-- "OpenAI launches GPT-6" score 0.6 and stay apart.
sameStory :: (Text, Set.Set Text) -> (Text, Set.Set Text) -> Bool
sameStory (na, ta) (nb, tb)
  | T.null na || T.null nb = False
  | na == nb               = True
  | Set.size ta < 3 || Set.size tb < 3 = False
  | otherwise              = jaccard ta tb >= 0.7

jaccard :: Set.Set Text -> Set.Set Text -> Double
jaccard a b
  | union' == 0 = 0
  | otherwise   = fromIntegral (Set.size (Set.intersection a b)) / fromIntegral union'
  where union' = Set.size (Set.union a b)

--------------------------------------------------------------------------------
-- Ordering
--------------------------------------------------------------------------------

-- | Oldest published first, by group, with each group's members contiguous.
--
-- Groups rather than individual candidates carry the ordering, because a group
-- is resolved as a unit and it is the group's earliest coverage that decides
-- where the story starts.
--
-- Undated candidates go last, which is a decision and not a fallback. Tavily
-- results usually have no date (its MCP server pins @topic@ to @general@,
-- which returns none), and treating dateless as oldest would let a search hit
-- create a story ahead of the dated coverage that actually broke it — giving
-- the story a wrong @start_date@ that no later run corrects. Last, the dated
-- coverage has already created the story, and the search hit resolves against
-- it as what it is: another copy.
orderForResolution :: [Candidate] -> [Candidate]
orderForResolution = concatMap (sortOn memberKey) . sortOn groupKey . byGroup
  where
    byGroup cs = M.elems (M.fromListWith (flip (<>)) [ (cndGroupKey c, [c]) | c <- cs ])

    groupKey grp = (isNothing earliest, earliest, cndGroupKey (headOf grp))
      where earliest = case mapMaybe (asPublishedAt . cndArticle) grp of
              []  -> Nothing
              ds  -> Just (minimum ds)

    memberKey c = (isNothing published, published, asUrlCanonical (cndArticle c))
      where published = asPublishedAt (cndArticle c)

    -- byGroup never produces an empty group; this is the total version of head.
    headOf (c:_) = c
    headOf []    = candidate emptySpec
    emptySpec = ArticleSpec "" "" "" Nothing "" Nothing Nothing "" Nothing

--------------------------------------------------------------------------------
-- Small shared helpers
--------------------------------------------------------------------------------

-- | ISO-8601 UTC, the one timestamp format in this program.
isoUTC :: UTCTime -> Text
isoUTC = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Blank and absent are the same thing everywhere here: feeds emit empty
-- elements for fields they do not have, and a @Just ""@ downstream is a value
-- that looks present and reads as missing.
nonEmpty :: Text -> Maybe Text
nonEmpty t = let s = T.strip t in if T.null s then Nothing else Just s
