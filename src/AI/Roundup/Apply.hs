{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Stage 5: the only code in the project that writes to the database.
--
-- __Why this takes a path and not a 'AI.Roundup.Store.Store'.__ "AI.Roundup.Store"
-- wraps its 'Database.SQLite.Simple.Connection' in a newtype and exports no way
-- to get it back out, so there is no function anywhere from a @Store@ to a
-- writable handle. Handing this module a @Store@ would either break that (a new
-- accessor, and then every reader has a writer in reach) or be a lie (the
-- handle would still be writable, just undocumented). Instead the two modules
-- never share a handle at all: they address the same database by path, Store
-- opens it to read, Apply opens it to write, and WAL mode — which @openStore@
-- sets — makes that concurrency fine. "Store reads, Apply writes" is then true
-- because the types make the other direction unreachable, not because callers
-- are careful.
--
-- The schema belongs to @openStore@, so 'applyChangeSet' refuses to run against
-- a database that has never been opened by it rather than carrying a second
-- copy of the DDL.
--
-- Two invariants are enforced here and nowhere else:
--
-- * @last_updated@ is derived as @max(article.published_at)@ after the day's
--   articles land. It is never read from the change set — a column that can be
--   set by an earlier stage is a column that can drift from the articles it is
--   supposed to describe.
--
-- * @start_date@ is a publication date, never a discovery date. On creation it
--   is the change set's, which the resolver anchored to earliest coverage.
--   Afterwards it only ever moves /earlier/, and only when the basis is
--   @earliest_coverage@ — backfilled older coverage should correct it, and a
--   later article never should.
--
-- Everything is idempotent, because the CLI promises that rerunning a day
-- changes nothing: stories are matched by slug before insert, articles collide
-- on the unique canonical URL, digests on the unique date, digest entries on
-- @(digest_id, story_id)@ and digest-article links on
-- @(digest_id, article_id)@. Applying one change set twice yields zero new
-- stories, zero new articles, zero new links and one digest row.

module AI.Roundup.Apply
  ( applyChangeSet
  , ApplyReport (..)
  ) where

import AI.Roundup.ChangeSet
  ( ArticleSpec (..), DayChangeSet (..), DigestEntrySpec (..)
  , NewStorySpec (..), StoryCompanySpec (..), StoryMeta (..)
  , StoryUpdateSpec (..) )
import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when)
import Data.Char (isAlphaNum, toLower, toUpper)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
       ( Connection, NamedParam (..), Only (..), changes, close, execute
       , executeNamed, execute_, lastInsertRowId, open, query, query_
       , withTransaction )

-- | What one application actually did. The counts are what the CLI's rerun
-- promise is checked against, and 'arpSkipped' is the only place a change set
-- that referred to something absent can announce itself.
data ApplyReport = ApplyReport
  { arpStoriesCreated   :: Int
  , arpStoriesUpdated   :: Int
    -- ^ Update specs applied, not stories whose text moved. Reapplying a day
    -- writes the same prose back, so this stays at the day's count while
    -- 'arpStoriesCreated' and 'arpArticlesInserted' both fall to zero.
  , arpArticlesInserted :: Int
  , arpArticlesLinked   :: Int
    -- ^ Articles pointed at by the day's digest. Not the same number as
    -- 'arpArticlesInserted': coverage already in the corpus under another story
    -- is linked without being inserted, so this is the count of what the day
    -- gathered where that is the count of what was new.
  , arpDigestCreated    :: Bool
  , arpEntriesWritten   :: Int
  , arpSkipped          :: [Text]
  } deriving (Eq, Show)

emptyReport :: ApplyReport
emptyReport = ApplyReport 0 0 0 0 False 0 []

-- | Apply a change set to the database at @path@.
--
-- One transaction: a change set is a day, and half a day in the corpus is worse
-- than none of it.
applyChangeSet :: FilePath -> DayChangeSet -> IO ApplyReport
applyChangeSet path cs =
  bracket (open path) close $ \conn -> do
    -- Both pragmas have to be outside the transaction. Foreign keys are off by
    -- default in SQLite, and the story/article/digest graph is only as sound as
    -- its references.
    execute_ conn "PRAGMA foreign_keys = ON"
    requireSchema conn
    withTransaction conn (applyAll conn cs)

-- | Refuse a database whose schema has never been created. @openStore@ owns the
-- DDL; duplicating it here would give the project two schemas to keep in step.
requireSchema :: Connection -> IO ()
requireSchema conn = do
  rows <- query_ conn
    "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'story'"
  case rows of
    [Only (n :: Int)] | n > 0 -> pure ()
    _ -> throwIO . userError $
      "apply: no 'story' table — open the database with Store.openStore first, \
      \which is what creates the schema"

-- | What applying one story spec did.
--
-- A record rather than the tuple this was, because the digest now needs
-- 'soArticles': the articles a spec actually put in the corpus, which is what
-- @digest_article@ links the day to. Collecting them here rather than walking
-- the change set again in 'applyDigest' is what keeps a story that could not be
-- applied from contributing coverage to the day — an update naming an unknown
-- story returns no articles, so the digest cannot point at articles that were
-- never stored.
data StoryOutcome = StoryOutcome
  { soTouched  :: Bool     -- ^ created, for a new story; found, for an update
  , soInserted :: Int      -- ^ articles new to the corpus
  , soArticles :: [Int64]  -- ^ every article now attached, new or colliding
  , soNotes    :: [Text]
  }

-- | What applying the digest half did. A record for the same reason: two
-- adjacent 'Int' fields in a tuple are two chances to return them the wrong way
-- round, and nothing at the call site would notice.
data DigestOutcome = DigestOutcome
  { doCreated :: Bool
  , doEntries :: Int
  , doLinked  :: Int
  , doNotes   :: [Text]
  }

applyAll :: Connection -> DayChangeSet -> IO ApplyReport
applyAll conn cs = do
  created <- mapM (applyNewStory conn) (dcsNewStories cs)
  updated <- mapM (applyUpdate conn) (dcsUpdates cs)
  let touched = created ++ updated
  digest <- applyDigest conn cs (concatMap soArticles touched)
  pure emptyReport
    { arpStoriesCreated   = length (filter soTouched created)
    , arpStoriesUpdated   = length (filter soTouched updated)
    , arpArticlesInserted = sum (map soInserted touched)
    , arpArticlesLinked   = doLinked digest
    , arpDigestCreated    = doCreated digest
    , arpEntriesWritten   = doEntries digest
    , arpSkipped          = concatMap soNotes touched ++ doNotes digest
    }

--------------------------------------------------------------------------------
-- New stories
--------------------------------------------------------------------------------

-- | A slug that is already present is treated as the same story rather than as
-- a conflict, and its prose is left alone. That is what makes a rerun a no-op,
-- and it also means a change set replayed after a hand edit cannot silently
-- revert the edit.
applyNewStory :: Connection -> NewStorySpec -> IO StoryOutcome
applyNewStory conn spec = do
  let meta = nssMeta spec
  existing <- storyIdForSlug conn (smSlug meta)
  (storyId, created) <- case existing of
    Just sid -> pure (sid, False)
    Nothing  -> (\sid -> (sid, True)) <$> insertStory conn spec
  articles <- mapM (insertArticle conn storyId) (nssArticles spec)
  refreshDates conn storyId
  pure StoryOutcome
    { soTouched  = created
    , soInserted = sum (map fst articles)
    , soArticles = [ i | (_, Just i) <- articles ]
    , soNotes    = [ "story already present, prose left as it was: " <> smSlug meta
                   | not created ]
    }

insertStory :: Connection -> NewStorySpec -> IO Int64
insertStory conn spec = do
  let meta = nssMeta spec
  categoryId <- ensureCategory conn (smCategory meta)
  companyIds <- forM (smCompanies meta) $ \c ->
    (,) <$> ensureCompany conn (scsName c) <*> pure (scsPrimary c)
  -- A model family hangs off a company, so a story with no company cannot have
  -- one; the family is dropped rather than attached to an invented owner.
  familyId <- case (smModelFamily meta, primaryOf companyIds) of
    (Just fam, Just owner) -> Just <$> ensureModelFamily conn owner fam
    _                      -> pure Nothing
  executeNamed conn
    "INSERT INTO story (slug, title, category_id, model_family_id, summary, \
    \                   writeup_md, start_date, start_date_basis) \
    \ VALUES (:slug, :title, :cat, :fam, :summary, :writeup, :start, :basis)"
    [ ":slug"    := smSlug meta
    , ":title"   := smTitle meta
    , ":cat"     := categoryId
    , ":fam"     := familyId
    , ":summary" := nssSummary spec
    , ":writeup" := nssWriteup spec
    , ":start"   := nonEmpty (smStartDate meta)
    , ":basis"   := nonEmpty (smStartBasis meta)
    ]
  storyId <- lastInsertRowId conn
  mapM_ (\(companyId, isPrimary) -> execute conn
          "INSERT OR IGNORE INTO story_company (story_id, company_id, is_primary) \
          \ VALUES (?, ?, ?)"
          (storyId, companyId, if isPrimary then 1 :: Int else 0))
        companyIds
  pure storyId

-- | The company a model family should hang off: the primary if one is marked,
-- otherwise the first, which is the resolver's most-confident guess.
primaryOf :: [(Int64, Bool)] -> Maybe Int64
primaryOf cs = listToMaybe ([ i | (i, True) <- cs ] ++ map fst cs)

--------------------------------------------------------------------------------
-- Updates to existing stories
--------------------------------------------------------------------------------

applyUpdate :: Connection -> StoryUpdateSpec -> IO StoryOutcome
applyUpdate conn spec = do
  found <- resolveStory conn (susStoryId spec) (susSlug spec)
  case found of
    Nothing -> pure StoryOutcome
      { soTouched  = False
      , soInserted = 0
      , soArticles = []
      , soNotes    = [ "update refers to an unknown story: " <> susSlug spec ]
      }
    Just storyId -> do
      articles <- mapM (insertArticle conn storyId) (susNewArticles spec)
      maybe (pure ()) (setWriteup conn storyId) (susWriteup spec)
      maybe (pure ()) (setSummary conn storyId) (susSummary spec)
      refreshDates conn storyId
      pure StoryOutcome
        { soTouched  = True
        , soInserted = sum (map fst articles)
        , soArticles = [ i | (_, Just i) <- articles ]
        , soNotes    = []
        }

-- | Prefer the id: it came from the resolver, which read it out of this
-- database in this run. The slug is the fallback, and the reason the change set
-- carries both — a draft applied to a rebuilt database has stale ids but stable
-- slugs.
resolveStory :: Connection -> Int64 -> Text -> IO (Maybe Int64)
resolveStory conn storyId slug = do
  byId <- query conn "SELECT id FROM story WHERE id = ?" (Only storyId)
  case byId of
    (Only i : _) -> pure (Just i)
    _            -> storyIdForSlug conn slug

-- Two functions rather than one taking a column name: the column would have to
-- be spliced into the SQL, and a spliced identifier is the shape of a mistake
-- even when the value is a literal three lines up.
setWriteup :: Connection -> Int64 -> Text -> IO ()
setWriteup conn storyId value =
  execute conn "UPDATE story SET writeup_md = ? WHERE id = ?" (value, storyId)

setSummary :: Connection -> Int64 -> Text -> IO ()
setSummary conn storyId value =
  execute conn "UPDATE story SET summary = ? WHERE id = ?" (value, storyId)

--------------------------------------------------------------------------------
-- Articles and derived dates
--------------------------------------------------------------------------------

-- | Insert one article, returning 1 if it was new, and the id it now has.
--
-- @url_canonical@ is unique, so an article already grouped under some other
-- story stays where it is: that collision /is/ the duplicate-coverage case, and
-- moving the article would split the group the resolver just made.
--
-- The id comes back on the collision path too, and has to: the day gathered
-- that coverage whether or not the corpus had seen it before, and a digest that
-- listed only the URLs that happened to be new would under-report a day where
-- two stories share an article.
insertArticle :: Connection -> Int64 -> ArticleSpec -> IO (Int, Maybe Int64)
insertArticle conn storyId a = do
  executeNamed conn
    "INSERT OR IGNORE INTO article \
    \ (story_id, url_canonical, url_original, title, outlet, source_id, guid, \
    \  published_at, fetched_at, excerpt) \
    \ VALUES (:story, :canon, :orig, :title, :outlet, :source, :guid, \
    \         :published, :fetched, :excerpt)"
    [ ":story"     := storyId
    , ":canon"     := asUrlCanonical a
    , ":orig"      := asUrlOriginal a
    , ":title"     := asTitle a
    , ":outlet"    := asOutlet a
    , ":source"    := asSourceId a
    , ":guid"      := asGuid a
    , ":published" := asPublishedAt a
    , ":fetched"   := asFetchedAt a
    , ":excerpt"   := asExcerpt a
    ]
  inserted <- changes conn
  articleId <- articleIdForUrl conn (asUrlCanonical a)
  pure (inserted, articleId)

-- | Re-derive the story's dates from its articles.
--
-- @last_updated@ is the maximum published date, always. @start_date@ is only
-- pulled earlier, and only for @earliest_coverage@: a story whose start date was
-- stated in a source outranks anything inferable from publication times. Both
-- are left alone when the articles carry no dates at all — an undated feed
-- should not be able to blank a date that is already right.
refreshDates :: Connection -> Int64 -> IO ()
refreshDates conn storyId = do
  rows <- query conn
    "SELECT MIN(published_at), MAX(published_at) FROM article WHERE story_id = ?"
    (Only storyId)
  case rows of
    [] -> pure ()
    ((mEarliest, mLatest) : _) -> do
      maybe (pure ())
            (\latest -> execute conn
               "UPDATE story SET last_updated = ? WHERE id = ?"
               (latest :: Text, storyId))
            mLatest
      maybe (pure ())
            (\earliest -> execute conn
               "UPDATE story SET start_date = ? \
               \ WHERE id = ? \
               \   AND (start_date_basis IS NULL \
               \        OR start_date_basis = 'earliest_coverage') \
               \   AND (start_date IS NULL OR start_date > ?)"
               (dayOf earliest, storyId, dayOf earliest))
            mEarliest

-- | @start_date@ is a day, not an instant; @published_at@ is an instant. ISO
-- ordering makes the truncation a plain prefix.
dayOf :: Text -> Text
dayOf = T.take 10

--------------------------------------------------------------------------------
-- The digest
--------------------------------------------------------------------------------

-- | The day's row, its story entries, and its links to the day's coverage.
--
-- @articleIds@ is every article the story half of this change set put in the
-- corpus, in the order the stories were applied. It is passed in rather than
-- recovered from the change set here, so that the only articles a digest can
-- point at are ones that were actually stored.
applyDigest :: Connection -> DayChangeSet -> [Int64] -> IO DigestOutcome
applyDigest conn cs articleIds = do
  existing <- query conn "SELECT id FROM digest WHERE digest_date = ?"
                (Only (dcsDate cs))
  (digestId, created) <- case existing of
    (Only i : _) -> pure (i, False)
    _ -> do
      execute conn "INSERT INTO digest (digest_date, intro_md) VALUES (?, ?)"
        (dcsDate cs, dcsIntro cs)
      (\i -> (i, True)) <$> lastInsertRowId conn
  -- Reapplying a day that has been recomposed should correct the intro; an
  -- intro-less change set should not erase one that is there.
  case dcsIntro cs of
    Just intro | not created ->
      execute conn "UPDATE digest SET intro_md = ? WHERE id = ?" (intro, digestId)
    _ -> pure ()
  results <- mapM (applyEntry conn digestId) (dcsDigestEntries cs)
  linked <- mapM (linkArticle conn digestId) articleIds
  pure DigestOutcome
    { doCreated = created
    , doEntries = sum (map fst results)
    , doLinked  = sum linked
    , doNotes   = concatMap snd results
    }

-- | Point the day at one article.
--
-- @INSERT OR IGNORE@ on @(digest_id, article_id)@, which does double duty: a
-- rerun links nothing new and reports zero, exactly as the story and article
-- counts do, and an article named twice in one change set — two stories sharing
-- a piece of coverage — is linked once and counted once without the caller
-- having to deduplicate first.
linkArticle :: Connection -> Int64 -> Int64 -> IO Int
linkArticle conn digestId articleId = do
  execute conn
    "INSERT OR IGNORE INTO digest_article (digest_id, article_id) VALUES (?, ?)"
    (digestId, articleId)
  changes conn

applyEntry :: Connection -> Int64 -> DigestEntrySpec -> IO (Int, [Text])
applyEntry conn digestId e = do
  found <- storyIdForSlug conn (desSlug e)
  case found of
    Nothing -> pure (0, [ "digest entry refers to an unknown story: "
                          <> desSlug e ])
    Just storyId -> do
      executeNamed conn
        "INSERT OR IGNORE INTO digest_entry \
        \ (digest_id, story_id, kind, delta_md, position) \
        \ VALUES (:digest, :story, :kind, :delta, :position)"
        [ ":digest"   := digestId
        , ":story"    := storyId
        , ":kind"     := desKind e
        , ":delta"    := desDeltaMd e
        , ":position" := desPosition e
        ]
      n <- changes conn
      pure (n, [])

--------------------------------------------------------------------------------
-- Resolving names and slugs to ids
--------------------------------------------------------------------------------

storyIdForSlug :: Connection -> Text -> IO (Maybe Int64)
storyIdForSlug conn slug = do
  rows <- query conn "SELECT id FROM story WHERE slug = ?" (Only slug)
  pure (fmap fromOnly (listToMaybe rows))

-- | The stored article at a canonical URL. Read after the insert rather than
-- taken from 'lastInsertRowId', which is only the right answer when the insert
-- was not ignored — and the ignored case is exactly the one whose id is least
-- guessable.
articleIdForUrl :: Connection -> Text -> IO (Maybe Int64)
articleIdForUrl conn url = do
  rows <- query conn "SELECT id FROM article WHERE url_canonical = ?" (Only url)
  pure (fmap fromOnly (listToMaybe rows))

-- | Categories are seeded by the schema, but a change set may name one that is
-- not, and losing the story is worse than gaining a category.
ensureCategory :: Connection -> Text -> IO Int64
ensureCategory conn slug = do
  rows <- query conn "SELECT id FROM category WHERE slug = ?" (Only slug)
  case rows of
    (Only i : _) -> pure i
    _ -> do
      execute conn "INSERT INTO category (slug, label) VALUES (?, ?)"
        (slug, labelFromSlug slug)
      lastInsertRowId conn

-- | Resolve a company /name as the model wrote it/ to a company id.
--
-- Matching is on the squashed form — lowercase, alphanumerics only — so
-- \"Open AI\", \"OpenAI\" and \"open-ai\" all land on the same row. That is the
-- whole job of @company_alias@: the model proposes a variant, the variant is
-- recorded against the company it matched, and every later lookup of that exact
-- spelling hits directly. The company table is small enough that reading it
-- whole beats trying to express the squash in SQL.
ensureCompany :: Connection -> Text -> IO Int64
ensureCompany conn name = do
  rows    <- query_ conn "SELECT id, slug, name FROM company"
                :: IO [(Int64, Text, Text)]
  aliases <- query_ conn "SELECT company_id, alias FROM company_alias"
                :: IO [(Int64, Text)]
  let key = squash name
      hits = [ (i, s, n) | (i, s, n) <- rows, squash s == key || squash n == key ]
          ++ [ (i, "", "") | (i, alias) <- aliases, squash alias == key ]
  case listToMaybe hits of
    Just (companyId, slug, stored) -> do
      -- Only a genuinely new spelling earns an alias row.
      when (name /= slug && name /= stored) $
        execute conn "INSERT OR IGNORE INTO company_alias (company_id, alias) \
                     \ VALUES (?, ?)" (companyId, name)
      pure companyId
    Nothing -> do
      -- A name with no alphanumerics at all slugifies to nothing; "company"
      -- plus a suffix is a worse slug than a real one but a better one than "".
      slug <- freeCompanySlug conn (orElse (slugify name) "company")
      execute conn "INSERT INTO company (slug, name) VALUES (?, ?)" (slug, name)
      companyId <- lastInsertRowId conn
      execute conn "INSERT OR IGNORE INTO company_alias (company_id, alias) \
                   \ VALUES (?, ?)" (companyId, name)
      pure companyId

ensureModelFamily :: Connection -> Int64 -> Text -> IO Int64
ensureModelFamily conn companyId slug = do
  rows <- query conn "SELECT id FROM model_family WHERE slug = ?" (Only slug)
  case rows of
    (Only i : _) -> pure i
    _ -> do
      execute conn
        "INSERT INTO model_family (company_id, slug, name) VALUES (?, ?, ?)"
        (companyId, slug, labelFromSlug slug)
      lastInsertRowId conn

-- | The given slug, or it with a numeric suffix, whichever is free. Only
-- reachable when two genuinely different companies squash apart but slugify
-- together, which is rare enough that a suffix is a better answer than a merge.
freeCompanySlug :: Connection -> Text -> IO Text
freeCompanySlug conn wanted = go (0 :: Int)
  where
    go n = do
      let candidate = if n == 0 then wanted else wanted <> "-" <> T.pack (show n)
      rows <- query conn "SELECT 1 FROM company WHERE slug = ?" (Only candidate)
      if null (rows :: [Only Int]) then pure candidate else go (n + 1)

--------------------------------------------------------------------------------
-- Text helpers
--------------------------------------------------------------------------------

-- | The matching key for company names: everything that is not a letter or
-- digit is noise, and case never distinguishes two companies.
squash :: Text -> Text
squash = T.filter isAlphaNum . T.toLower

-- | A URL-safe slug: alphanumerics kept, everything else collapsed to a single
-- hyphen.
slugify :: Text -> Text
slugify =
  T.dropAround (== '-')
  . T.pack
  . reverse
  . foldl' step []
  . T.unpack
  where
    step acc c
      | isAlphaNum c        = toLower c : acc
      | take 1 acc == "-"   = acc
      | otherwise           = '-' : acc

-- | A display label for a slug that arrived without one: @general_tech@ becomes
-- @General Tech@. Rough on purpose — a hand-seeded label always wins, because
-- the seed row is found before this is reached.
labelFromSlug :: Text -> Text
labelFromSlug =
  T.unwords . map capitalise . T.words . T.map spacer
  where
    spacer c = if c == '_' || c == '-' then ' ' else c
    capitalise w = case T.uncons w of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> w

-- | Empty text in a change set means "not stated", which the column spells NULL.
nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null (T.strip t) then Nothing else Just t

orElse :: Text -> Text -> Text
orElse t fallback = if T.null t then fallback else t
