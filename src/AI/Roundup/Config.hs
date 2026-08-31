{-# LANGUAGE OverloadedStrings #-}

-- | What to read: @sources.yaml@, parsed into typed config.
--
-- Sources are configuration rather than code because the set of feeds is the
-- part of this program most likely to change without a rebuild, and because a
-- YAML file can be commented for a reader who is adding a feed and has no
-- interest in the parser.
--
-- Two kinds of source, deliberately kept as separate lists rather than one
-- list with a @type:@ tag: they have nothing in common but an id, and a sum
-- type here would mean every consumer branching on a tag it already knows
-- statically ("AI.Roundup.Ingest.Feed" handles one, "AI.Roundup.Ingest.Tavily"
-- the other).
--
-- The @id@ of a source is written into @article.source_id@ and is the only
-- thing that survives into the database, so it must be stable across edits:
-- change a feed's URL freely, change its id and the provenance of everything
-- already ingested from it goes stale.

module AI.Roundup.Config
  ( -- * Config
    SourcesConfig (..)
  , FeedSource (..)
  , TavilySource (..)
    -- * Loading
  , defaultSourcesPath
  , readSourcesConfig
    -- * Inspection
  , sourceIds
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?), (.!=))
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

-- | Where @run@ looks when @--sources@ is not given. Relative to the working
-- directory, next to @schema.sql@.
defaultSourcesPath :: FilePath
defaultSourcesPath = "sources.yaml"

-- | An RSS or Atom feed.
data FeedSource = FeedSource
  { fsId     :: Text
  , fsUrl    :: Text
  , fsOutlet :: Maybe Text
    -- ^ Who published the articles this feed carries, when that is a fixed
    -- answer — a vendor blog only ever carries its own posts. Left out for
    -- aggregators, where the outlet differs per item and is derived from the
    -- article's host instead (see 'AI.Roundup.Ingest.outletFromUrl').
  } deriving (Eq, Show)

instance FromJSON FeedSource where
  parseJSON = withObject "feed source" $ \o -> FeedSource
    <$> o .:  "id"
    <*> o .:  "url"
    <*> o .:? "outlet"

-- | A standing Tavily search. The fields mirror the @tavily_search@ MCP tool's
-- arguments; everything optional here is simply omitted from the call, so the
-- server's own defaults apply rather than a second set of defaults invented
-- here.
data TavilySource = TavilySource
  { tsId             :: Text
  , tsQuery          :: Text
  , tsMaxResults     :: Int
  , tsTimeRange      :: Maybe Text   -- ^ @day@ | @week@ | @month@ | @year@
  , tsSearchDepth    :: Maybe Text   -- ^ @basic@ | @advanced@ | @fast@
  , tsIncludeDomains :: [Text]
  , tsExcludeDomains :: [Text]
  } deriving (Eq, Show)

instance FromJSON TavilySource where
  parseJSON = withObject "tavily source" $ \o -> TavilySource
    <$> o .:  "id"
    <*> o .:  "query"
    <*> o .:? "max_results"     .!= 10
    <*> o .:? "time_range"
    <*> o .:? "search_depth"
    <*> o .:? "include_domains" .!= []
    <*> o .:? "exclude_domains" .!= []

-- | The whole file.
data SourcesConfig = SourcesConfig
  { cfgMaxAgeDays :: Maybe Int
    -- ^ Ignore articles published longer ago than this. Not a nicety: a feed
    -- carrying its entire archive (openai.com's does, back to 2015) is not an
    -- error and cannot be told apart from a burst of real news by anything
    -- mechanical, so without a window the first run resolves a decade of posts
    -- as today's stories. 'Nothing' means no window, which is right for a
    -- fixture and wrong for a daily run.
  , cfgFeeds      :: [FeedSource]
  , cfgTavily     :: [TavilySource]
  } deriving (Eq, Show)

instance FromJSON SourcesConfig where
  parseJSON = withObject "sources.yaml" $ \o -> SourcesConfig
    <$> o .:? "max_age_days"
    <*> o .:? "feeds"  .!= []
    <*> o .:? "tavily" .!= []

-- | Every source id in the file, feeds first, in file order.
sourceIds :: SourcesConfig -> [Text]
sourceIds cfg = map fsId (cfgFeeds cfg) <> map tsId (cfgTavily cfg)

-- | Load and validate @sources.yaml@.
--
-- 'Left' rather than an exception, and a 'Text' rather than a
-- 'Yaml.ParseException', because the only sensible response at the call site
-- is to print the message and stop: a run with a half-understood source list
-- would silently ingest the wrong corpus.
readSourcesConfig :: FilePath -> IO (Either Text SourcesConfig)
readSourcesConfig path = do
  parsed <- Yaml.decodeFileEither path
  pure $ case parsed of
    Left err  -> Left $ T.pack path <> ": "
                     <> T.pack (Yaml.prettyPrintParseException err)
    Right cfg -> validate path cfg

-- | Reject the two mistakes that are silent rather than loud: an empty file
-- (a run that ingests nothing looks like a quiet day) and duplicate ids (two
-- sources writing the same @article.source_id@ makes provenance a lie).
validate :: FilePath -> SourcesConfig -> Either Text SourcesConfig
validate path cfg
  | null ids            = Left $ T.pack path <> ": no sources configured"
  | any T.null ids      = Left $ T.pack path <> ": every source needs a non-empty id"
  | not (null repeated) = Left $ T.pack path <> ": duplicate source ids: "
                              <> T.intercalate ", " repeated
  | otherwise           = Right cfg
  where
    ids      = sourceIds cfg
    repeated = duplicates (sort ids)

duplicates :: Eq a => [a] -> [a]
duplicates xs = [ a | (a, b) <- zip xs (drop 1 xs), a == b ]
