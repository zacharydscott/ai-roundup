{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The change-set spine.
--
-- A 'DayChangeSet' is the single typed record that flows from composition into
-- application, serialised to @data\/drafts\/<date>\/changes.json@ before anything
-- touches the database. It is deliberately plain data: every field is a value
-- that reads sensibly on its own (company names, category slugs, article URLs),
-- so the JSON artefact is readable without the schema open in front of you.
--
-- Application ("AI.Roundup.Apply") is the only code that turns these into rows,
-- and it must resolve slugs/names to ids and derive @last_updated@ itself —
-- nothing here is allowed to carry a database id for a row that does not yet
-- exist, because that id is only known at apply time.

module AI.Roundup.ChangeSet
  ( -- * Top-level change set
    DayChangeSet (..)
    -- * Story records
  , NewStorySpec (..)
  , StoryMeta (..)
  , StoryCompanySpec (..)
  , StoryUpdateSpec (..)
  , ArticleSpec (..)
  , DigestEntrySpec (..)
    -- * JSON file I/O
  , readChangeSet
  , writeChangeSet
    -- * Well-known digest kinds
  , kindBreaking
  , kindUpdate
  ) where

import Data.Aeson
  ( FromJSON (..), ToJSON (..), Value (..), eitherDecodeFileStrict, encode
  , genericParseJSON, genericToJSON, defaultOptions, fieldLabelModifier, camelTo2
  , Options )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isLower)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (intersperse, sortOn)
import Data.Text (Text)
import GHC.Generics (Generic)

-- JSON field names are snake_case with the record prefix stripped, so
-- @dcsNewStories@ -> @new_stories@ and @asUrlCanonical@ -> @url_canonical@ —
-- closer to the column names and nicer to read raw.
aesonOpts :: Options
aesonOpts = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropWhile isLower }

-- | A company attached to a story. Referenced by name (the apply stage resolves
-- the name to a @company@ id, creating the row if needed); @is_primary@ marks
-- the one company that single-company display queries read.
data StoryCompanySpec = StoryCompanySpec
  { scsName    :: Text
  , scsPrimary :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON StoryCompanySpec where toJSON = genericToJSON aesonOpts
instance FromJSON StoryCompanySpec where parseJSON = genericParseJSON aesonOpts

-- | One article of coverage. This is the article table, pre-id: the canonical
-- URL is the uniqueness key the apply stage relies on for idempotency.
data ArticleSpec = ArticleSpec
  { asUrlCanonical :: Text
  , asUrlOriginal  :: Text
  , asTitle        :: Text
  , asOutlet       :: Maybe Text
  , asSourceId     :: Text
  , asGuid         :: Maybe Text
  , asPublishedAt  :: Maybe Text   -- ^ ISO-8601 UTC, from the feed/page
  , asFetchedAt    :: Text         -- ^ ISO-8601 UTC, our clock
  , asExcerpt      :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ArticleSpec where toJSON = genericToJSON aesonOpts
instance FromJSON ArticleSpec where parseJSON = genericParseJSON aesonOpts

-- | The semantic facts about a story that the resolver decides and the writer
-- turns into prose: slug, title, taxonomy, companies, and the anchored dates.
data StoryMeta = StoryMeta
  { smSlug        :: Text
  , smTitle       :: Text
  , smCategory    :: Text          -- ^ category slug
  , smModelFamily :: Maybe Text    -- ^ model family slug
  , smCompanies   :: [StoryCompanySpec]
  , smStartDate   :: Text          -- ^ publication date, never discovery date
  , smStartBasis  :: Text          -- ^ 'earliest_coverage' | 'stated_in_source'
  } deriving (Eq, Show, Generic)

instance ToJSON StoryMeta where toJSON = genericToJSON aesonOpts
instance FromJSON StoryMeta where parseJSON = genericParseJSON aesonOpts

-- | A brand-new story: the resolver's metadata plus the composed prose and the
-- articles it was grouped from.
data NewStorySpec = NewStorySpec
  { nssMeta     :: StoryMeta
  , nssSummary  :: Maybe Text      -- ^ one-paragraph standing summary
  , nssWriteup  :: Text            -- ^ full evolving prose
  , nssArticles :: [ArticleSpec]
  } deriving (Eq, Show, Generic)

instance ToJSON NewStorySpec where toJSON = genericToJSON aesonOpts
instance FromJSON NewStorySpec where parseJSON = genericParseJSON aesonOpts

-- | An update to an existing story: what changed today plus any new articles.
data StoryUpdateSpec = StoryUpdateSpec
  { susStoryId     :: Int64
  , susSlug        :: Text
  , susDeltaMd     :: Maybe Text   -- ^ what was added this day (digest content)
  , susWriteup     :: Maybe Text   -- ^ the rewritten full write-up, if any
  , susSummary     :: Maybe Text   -- ^ the rewritten standing summary, if any
  , susNewArticles :: [ArticleSpec]
  } deriving (Eq, Show, Generic)

instance ToJSON StoryUpdateSpec where toJSON = genericToJSON aesonOpts
instance FromJSON StoryUpdateSpec where parseJSON = genericParseJSON aesonOpts

-- | One row of the digest: which story, whether it broke or was updated, and
-- the delta prose that gives "additions to existing entries" real content.
data DigestEntrySpec = DigestEntrySpec
  { desSlug     :: Text          -- ^ story slug (resolved to id at apply time)
  , desKind     :: Text          -- ^ 'breaking' | 'update'
  , desDeltaMd  :: Maybe Text
  , desPosition :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON DigestEntrySpec where toJSON = genericToJSON aesonOpts
instance FromJSON DigestEntrySpec where parseJSON = genericParseJSON aesonOpts

-- | Everything one run decided to change, keyed by discovery date.
data DayChangeSet = DayChangeSet
  { dcsDate          :: Text        -- ^ discovery date, YYYY-MM-DD
  , dcsIntro         :: Maybe Text  -- ^ digest introduction
  , dcsNewStories    :: [NewStorySpec]
  , dcsUpdates       :: [StoryUpdateSpec]
  , dcsDigestEntries :: [DigestEntrySpec]
  } deriving (Eq, Show, Generic)

instance ToJSON DayChangeSet where toJSON = genericToJSON aesonOpts
instance FromJSON DayChangeSet where parseJSON = genericParseJSON aesonOpts

-- | Digest entry kinds. Kept as named constants so callers do not hand-roll
-- the strings.
kindBreaking, kindUpdate :: Text
kindBreaking = "breaking"
kindUpdate = "update"

-- | Read a change set back from disk (used by @apply <date>@ and the editor
-- loop).
readChangeSet :: FilePath -> IO (Either String DayChangeSet)
readChangeSet = eitherDecodeFileStrict

-- | Write a change set to disk (used by @run@, before anything is applied).
--
-- Indented rather than compact, which is not a cosmetic choice: this file is
-- the artefact the whole design rests on. It is read by a person before it is
-- applied, hand-edited in the deferred editor loop, and committed, so it has to
-- diff line by line and be legible without the schema open alongside it.
-- @aeson@'s own 'encode' writes one unbounded line and @aeson-pretty@ is not in
-- this project's dependency closure, so the thirty lines below are cheaper than
-- the dependency.
writeChangeSet :: FilePath -> DayChangeSet -> IO ()
writeChangeSet path cs =
  BL.writeFile path (B.toLazyByteString (prettyJson 0 (toJSON cs) <> "\n"))

-- | Render a 'Value' with two-space indentation.
--
-- Keys are sorted rather than left in the order the generic instance produced
-- them. Record order would read marginally better, but @aeson@'s key map does
-- not preserve it, and between an arbitrary order and a stable one the stable
-- one wins: two runs of the same day must produce files that differ only where
-- the content differs.
--
-- Scalars go through 'encode' rather than being rendered here, because string
-- escaping is the one part of JSON that is easy to get subtly wrong and the
-- write-ups are full of quotes, newlines and em dashes.
prettyJson :: Int -> Value -> B.Builder
prettyJson depth value = case value of
  Object o -> case sortOn fst (KeyMap.toList o) of
    []  -> "{}"
    kvs -> wrap "{" "}" [ key k <> ": " <> prettyJson (depth + 1) v | (k, v) <- kvs ]
  Array a -> case toList a of
    []  -> "[]"
    xs  -> wrap "[" "]" (map (prettyJson (depth + 1)) xs)
  scalar -> lazy (encode scalar)
  where
    key k = lazy (encode (Key.toText k))
    lazy = B.lazyByteString
    indent n = B.stringUtf8 (replicate (2 * n) ' ')
    wrap open close items = mconcat
      [ open, "\n"
      , mconcat (intersperse ",\n" [ indent (depth + 1) <> i | i <- items ])
      , "\n", indent depth, close
      ]
