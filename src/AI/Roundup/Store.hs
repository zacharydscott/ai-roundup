{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The story store.
--
-- Owns the schema (embedded 'schema.sql', applied at open with
-- @IF NOT EXISTS@) and exposes typed, read-only queries for the pipeline and
-- the CLI. Everything that writes to the database lives in
-- "AI.Roundup.Apply": this module opens the connection, creates the schema,
-- and reads. That split is deliberate — the resolver sub-agent gets narrow
-- typed reads (see "AI.Roundup.Resolve"), not a raw handle, and no read path
-- can mutate the corpus.
--
-- Dates are stored as ISO-8601 UTC text so comparisons and the
-- "start_date == earliest article" invariant are plain string comparisons.

module AI.Roundup.Store
  ( -- * Connection
    Store
  , openStore
  , closeStore

  -- * Categories
  , CategoryRow (..)
  , categories

  -- * Companies
  , CompanyRef (..)
  , companies
  , companyByName

  -- * Stories
  , StoryQuery (..)
  , StorySummaryRow (..)
  , StoryDetailRow (..)
  , StoryRelationRow (..)
  , StoryCompanyRow (..)
  , ArticleRow (..)
  , storiesMatching
  , storyDetail
  , storyDetailForSlug

  -- * Articles
  , articleForUrl

  -- * Listing
  , ListFilter (..)
  , StoryListItem (..)
  , listStories

  -- * Digests
  , DigestRow (..)
  , DigestEntryRow (..)
  , DigestArticleRow (..)
  , digestForDate
  , digestEntries
  , digestArticles
  , digests

  -- * Counting
  , storyCount
  , articleCount
  ) where

import Database.SQLite.Simple
       ( Connection, close, open, query, query_, queryNamed, execute_
       , Only (..), FromRow (..), field, NamedParam (..), Query (..)
       -- sqlite-simple's FromRow tuples stop at ten columns and the article
       -- projection is already ten; ':.' is how the story slug rides along
       -- beside it without a bespoke row type.
       , (:.) (..) )
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- The whole schema, embedded at build time and applied on every open.
schemaSql :: BS.ByteString
schemaSql = $(embedFile "schema.sql")

-- | An open connection. A newtype so callers cannot run ad-hoc SQL against a
-- schema this module owns; the resolver sub-agent gets narrow typed tools, not
-- this handle.
newtype Store = Store Connection

-- | Open the store at @path@, creating the schema (and seeded categories) if
-- absent. WAL mode: a run that holds the DB while composing prose does not
-- block a concurrent @list@ or @show@.
openStore :: FilePath -> IO Store
openStore path = do
  conn <- open path
  execute_ conn (Query "PRAGMA journal_mode = WAL")
  applySchema conn schemaSql
  pure (Store conn)

-- | Close the store.
closeStore :: Store -> IO ()
closeStore (Store conn) = close conn

-- | Run every statement in the embedded schema against an open connection.
-- The schema is split on @;@ after stripping full-line @--@ comments, so
-- multi-line @CREATE TABLE@ bodies are executed as single statements.
applySchema :: Connection -> BS.ByteString -> IO ()
applySchema conn bs =
  mapM_ (execute_ conn . Query) (splitStatements (TE.decodeUtf8 bs))

-- | Split a SQL script into individual statements.
--
-- Comment lines are dropped before splitting so a @;@ inside a comment (the
-- schema prose has a few) cannot fragment a statement. Then each @;@-delimited
-- chunk is trimmed; empty chunks are dropped.
splitStatements :: Text -> [Text]
splitStatements =
  filter (not . T.null)
  . map trim
  . T.splitOn ";"
  . T.unlines
  . filter (not . T.isPrefixOf "--")
  . map trim
  . T.lines

-- Newlines count as whitespace here: statements are split on @;@ after the
-- script has been re-joined with 'T.unlines', so every chunk but the first
-- begins with one, and the chunk after the final @;@ is nothing else. Leave
-- them in and that last chunk reaches SQLite as an empty query string, which
-- it rejects — every open would fail.
trim :: Text -> Text
trim = T.dropAround (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

--------------------------------------------------------------------------------
-- Categories
--------------------------------------------------------------------------------

data CategoryRow = CategoryRow
  { categorySlug  :: Text
  , categoryLabel :: Text
  } deriving (Eq, Show)

instance FromRow CategoryRow where
  fromRow = CategoryRow <$> field <*> field

categories :: Store -> IO [CategoryRow]
categories (Store conn) = query_ conn "SELECT slug, label FROM category"

--------------------------------------------------------------------------------
-- Companies
--------------------------------------------------------------------------------

data CompanyRef = CompanyRef
  { companyId      :: Int64
  , companySlug    :: Text
  , companyName    :: Text
  , companyAliases :: [Text]
  } deriving (Eq, Show)

-- | All companies with their aliases.
companies :: Store -> IO [CompanyRef]
companies (Store conn) = do
  rows <- query_ conn
    "SELECT c.id, c.slug, c.name, \
    \       (SELECT group_concat(alias, CHAR(1)) \
    \         FROM company_alias WHERE company_id = c.id) \
    \       FROM company c"
  pure [ CompanyRef i s n (maybe [] (T.splitOn "\x01") m)
       | (i, s, n, m) <- rows ]

-- | Look up a company by name, slug, or any of its aliases (case-insensitive).
companyByName :: Store -> Text -> IO (Maybe CompanyRef)
companyByName (Store conn) name = do
  rows <- query conn
    "SELECT c.id, c.slug, c.name, \
    \       (SELECT group_concat(alias, CHAR(1)) \
    \         FROM company_alias WHERE company_id = c.id) \
    \       FROM company c \
    \       WHERE LOWER(c.name) = LOWER(?) OR LOWER(c.slug) = LOWER(?) \
    \          OR c.id IN (SELECT company_id FROM company_alias \
    \                      WHERE LOWER(alias) = LOWER(?)) \
    \       LIMIT 1"
    (name, name, name)
  case rows of
    [] -> pure Nothing
    ((i, s, n, m):_) -> pure (Just (CompanyRef i s n (maybe [] (T.splitOn "\x01") m)))

--------------------------------------------------------------------------------
-- Stories: summary rows for the resolver's search_stories
--------------------------------------------------------------------------------

-- | Narrow query shape for the resolver's 'search_stories' tool.
data StoryQuery = StoryQuery
  { sqText    :: Maybe Text  -- ^ free-text LIKE over title + summary
  , sqCompany :: Maybe Text  -- ^ company name / slug / alias
  , sqCategory :: Maybe Text -- ^ category slug
  , sqSince   :: Maybe Text  -- ^ ISO date; keep stories with last_updated >= this
  , sqLimit   :: Int
  } deriving (Eq, Show)

data StorySummaryRow = StorySummaryRow
  { ssrId          :: Int64
  , ssrSlug        :: Text
  , ssrTitle       :: Text
  , ssrCategory    :: Text
  , ssrCompanies   :: [Text]
  , ssrStartDate   :: Maybe Text
  , ssrLastUpdated :: Maybe Text
  , ssrSummary     :: Maybe Text
  } deriving (Eq, Show)

-- | Stories matching the query, most recently updated first, as compact rows.
storiesMatching :: Store -> StoryQuery -> IO [StorySummaryRow]
storiesMatching (Store conn) q = do
  let (sql, params) = storyQuery q
  rows <- queryNamed conn (Query sql) params
  pure [ StorySummaryRow id_ slug title cat
             (maybe [] (map T.strip . T.splitOn ", ") mCompanies)
             mStart mLast mSummary
       | (id_, slug, title, cat, mCompanies, mStart, mLast, mSummary) <- rows ]

-- | Build the parameterised search SQL and its named parameters. Free-text,
-- company, category, and since are all optional; an empty WHERE keeps the most
-- recently updated stories. Repeated parameters get distinct names because a
-- named parameter cannot be bound more than once.
storyQuery :: StoryQuery -> (Text, [NamedParam])
storyQuery q =
  ( T.concat
      [ "SELECT s.id, s.slug, s.title, c.slug, "
      , "       (SELECT group_concat(co.name, ', ') "
      , "         FROM story_company sc JOIN company co ON co.id = sc.company_id "
      , "         WHERE sc.story_id = s.id), "
      , "       s.start_date, s.last_updated, s.summary "
      , " FROM story s "
      , " JOIN category c ON c.id = s.category_id "
      , " WHERE 1=1"
      , maybe "" (const " AND (s.title LIKE :text OR s.summary LIKE :text)") (sqText q)
      , maybe "" (const companyClause) (sqCompany q)
      , maybe "" (const " AND c.slug = :cat") (sqCategory q)
      , maybe "" (const " AND s.last_updated IS NOT NULL AND s.last_updated >= :since") (sqSince q)
      , " ORDER BY s.last_updated IS NULL, s.last_updated DESC, s.id LIMIT :lim"
      ]
  , concat
      [ maybe [] (\t -> [":text" := ("%" <> t <> "%")]) (sqText q)
      , maybe [] (\c -> [":cname" := c, ":cslug" := c, ":calias" := c]) (sqCompany q)
      , maybe [] (\cat -> [":cat" := cat]) (sqCategory q)
      , maybe [] (\s -> [":since" := s]) (sqSince q)
      , [":lim" := sqLimit q]
      ]
  )
  where
    companyClause =
      " AND s.id IN (SELECT story_id FROM story_company sc \
      \ JOIN company co ON co.id = sc.company_id \
      \ WHERE LOWER(co.name) = LOWER(:cname) OR LOWER(co.slug) = LOWER(:cslug) \
      \    OR co.id IN (SELECT company_id FROM company_alias \
      \                 WHERE LOWER(alias) = LOWER(:calias)))"

--------------------------------------------------------------------------------
-- Stories: full detail for get_story
--------------------------------------------------------------------------------

data StoryCompanyRow = StoryCompanyRow
  { scrCompanyId :: Int64
  , scrName      :: Text
  , scrPrimary   :: Bool
  } deriving (Eq, Show)

data StoryRelationRow = StoryRelationRow
  { srrRelatedSlug :: Text
  , srrRelation    :: Text
  , srrNote        :: Maybe Text
  } deriving (Eq, Show)

data StoryDetailRow = StoryDetailRow
  { sdrId          :: Int64
  , sdrSlug        :: Text
  , sdrTitle       :: Text
  , sdrCategory    :: Text
  , sdrModelFamily :: Maybe Text
  , sdrSummary     :: Maybe Text
  , sdrWriteup     :: Maybe Text
  , sdrStartDate   :: Maybe Text
  , sdrStartBasis  :: Maybe Text
  , sdrLastUpdated :: Maybe Text
  , sdrCompanies   :: [StoryCompanyRow]
  , sdrRelations   :: [StoryRelationRow]
  , sdrArticles    :: [ArticleRow]
  } deriving (Eq, Show)

data ArticleRow = ArticleRow
  { artId          :: Int64
  , artUrlCanonical :: Text
  , artUrlOriginal :: Maybe Text
  , artTitle       :: Maybe Text
  , artOutlet      :: Maybe Text
  , artSourceId    :: Maybe Text
  , artGuid        :: Maybe Text
  , artPublishedAt :: Maybe Text
  , artFetchedAt   :: Maybe Text
  , artExcerpt     :: Maybe Text
  } deriving (Eq, Show)

-- | The full story: fields, companies, relations, and its articles (oldest first).
storyDetail :: Store -> Int64 -> IO (Maybe StoryDetailRow)
storyDetail (Store conn) id_ = do
  top <- query conn
    "SELECT s.id, s.slug, s.title, c.slug, m.name, s.summary, s.writeup_md, \
    \       s.start_date, s.start_date_basis, s.last_updated \
    \  FROM story s \
    \  JOIN category c ON c.id = s.category_id \
    \  LEFT JOIN model_family m ON m.id = s.model_family_id \
    \  WHERE s.id = ?"
    (Only id_)
  case top of
    [] -> pure Nothing
    ((sid, slug, title, cat, mFamily, mSummary, mWriteup, mStart, mBasis, mLast):_) -> do
      comps <- query conn
        "SELECT co.id, co.name, sc.is_primary \
    \   FROM story_company sc JOIN company co ON co.id = sc.company_id \
    \   WHERE sc.story_id = ? ORDER BY sc.is_primary DESC, co.name"
        (Only id_)
      rels <- query conn
        "SELECT s2.slug, sr.relation, sr.note \
    \   FROM story_relation sr JOIN story s2 ON s2.id = sr.related_story_id \
    \   WHERE sr.story_id = ?"
        (Only id_)
      arts <- query conn
        "SELECT id, url_canonical, url_original, title, outlet, source_id, \
    \          guid, published_at, fetched_at, excerpt \
    \   FROM article WHERE story_id = ? \
    \   ORDER BY published_at IS NULL, published_at, id"
        (Only id_)
      pure (Just $ StoryDetailRow
        { sdrId = sid, sdrSlug = slug, sdrTitle = title, sdrCategory = cat
        , sdrModelFamily = mFamily, sdrSummary = mSummary, sdrWriteup = mWriteup
        , sdrStartDate = mStart, sdrStartBasis = mBasis, sdrLastUpdated = mLast
        , sdrCompanies = map (\(ci, cn, prim) -> StoryCompanyRow ci cn (prim /= (0 :: Int))) comps
        , sdrRelations = map (\(sl, rel, note) -> StoryRelationRow sl rel note) rels
        , sdrArticles  = map articleFromTuple arts
        })

-- | The same, addressed the way a reader addresses a story: by the slug that
-- reaches the URL of its page.
--
-- Ids are an implementation detail of the corpus — they appear in the resolver's
-- tool results and in a change set, and nowhere a person types. @show story@ and
-- the site generator both start from a slug, and without this the only route
-- from one to the other is listing every story and scanning for it.
storyDetailForSlug :: Store -> Text -> IO (Maybe StoryDetailRow)
storyDetailForSlug store@(Store conn) slug = do
  rows <- query conn "SELECT id FROM story WHERE slug = ?" (Only slug)
  case rows of
    (Only sid : _) -> storyDetail store sid
    _              -> pure Nothing

articleFromTuple :: (Int64, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text,
                     Maybe Text, Maybe Text, Maybe Text, Maybe Text) -> ArticleRow
articleFromTuple (i, uco, uorig, t, o, sid, g, pub, fetch, ex) =
  ArticleRow i uco uorig t o sid g pub fetch ex

--------------------------------------------------------------------------------
-- Articles
--------------------------------------------------------------------------------

-- | The stored article at a canonical URL, if any.
articleForUrl :: Store -> Text -> IO (Maybe ArticleRow)
articleForUrl (Store conn) url = do
  rows <- query conn
    "SELECT id, url_canonical, url_original, title, outlet, source_id, \
    \       guid, published_at, fetched_at, excerpt \
    \  FROM article WHERE url_canonical = ?"
    (Only url)
  pure (fmap articleFromTuple (listToMaybe rows))

--------------------------------------------------------------------------------
-- Listing
--------------------------------------------------------------------------------

data ListFilter = ListFilter
  { lfCompany  :: Maybe Text
  , lfCategory :: Maybe Text
  , lfSince    :: Maybe Text
  , lfLimit    :: Int
  } deriving (Eq, Show)

data StoryListItem = StoryListItem
  { litTitle       :: Text
  , litSlug        :: Text
  , litCategory    :: Text
  , litCompanies   :: [Text]
  , litStartDate   :: Maybe Text
  , litLastUpdated :: Maybe Text
  } deriving (Eq, Show)

listStories :: Store -> ListFilter -> IO [StoryListItem]
listStories (Store conn) f = do
  let (sql, params) = listQuery f
  rows <- queryNamed conn (Query sql) params
  pure [ StoryListItem title slug cat
             (maybe [] (map T.strip . T.splitOn ", ") mCompanies)
             mStart mLast
       | (title, slug, cat, mCompanies, mStart, mLast) <- rows ]

listQuery :: ListFilter -> (Text, [NamedParam])
listQuery f =
  ( T.concat
      [ "SELECT s.title, s.slug, c.slug, "
      , "       (SELECT group_concat(co.name, ', ') "
      , "         FROM story_company sc JOIN company co ON co.id = sc.company_id "
      , "         WHERE sc.story_id = s.id), "
      , "       s.start_date, s.last_updated "
      , " FROM story s "
      , " JOIN category c ON c.id = s.category_id "
      , " WHERE 1=1"
      , maybe "" (const companyClause) (lfCompany f)
      , maybe "" (const " AND c.slug = :cat") (lfCategory f)
      , maybe "" (const " AND s.last_updated IS NOT NULL AND s.last_updated >= :since") (lfSince f)
      , " ORDER BY s.last_updated IS NULL, s.last_updated DESC, s.id LIMIT :lim"
      ]
  , concat
      [ maybe [] (\c -> [":cname" := c, ":cslug" := c, ":calias" := c]) (lfCompany f)
      , maybe [] (\cat -> [":cat" := cat]) (lfCategory f)
      , maybe [] (\s -> [":since" := s]) (lfSince f)
      , [":lim" := lfLimit f]
      ]
  )
  where
    companyClause =
      " AND s.id IN (SELECT story_id FROM story_company sc \
      \ JOIN company co ON co.id = sc.company_id \
      \ WHERE LOWER(co.name) = LOWER(:cname) OR LOWER(co.slug) = LOWER(:cslug) \
      \    OR co.id IN (SELECT company_id FROM company_alias \
      \                 WHERE LOWER(alias) = LOWER(:calias)))"

--------------------------------------------------------------------------------
-- Digests
--------------------------------------------------------------------------------

data DigestRow = DigestRow
  { digestId    :: Int64
  , digestDate  :: Text
  , digestIntro :: Maybe Text
  } deriving (Eq, Show)

data DigestEntryRow = DigestEntryRow
  { derStorySlug :: Text
  , derKind      :: Text
  , derDelta     :: Maybe Text
  , derPosition  :: Int
  } deriving (Eq, Show)

-- | Every digest in the corpus, newest day first. What the site generator walks
-- to rebuild the archive, and the only place the corpus is asked for days
-- rather than for one day.
digests :: Store -> IO [DigestRow]
digests (Store conn) = do
  rows <- query_ conn
    "SELECT id, digest_date, intro_md FROM digest ORDER BY digest_date DESC"
  pure [ DigestRow i d intro | (i, d, intro) <- rows ]

digestForDate :: Store -> Text -> IO (Maybe DigestRow)
digestForDate (Store conn) date = do
  rows <- query conn "SELECT id, digest_date, intro_md FROM digest WHERE digest_date = ?"
    (Only date)
  pure (fmap (\(i, d, intro) -> DigestRow i d intro) (listToMaybe rows))

-- | One piece of the day's coverage, and the story it landed under.
--
-- The slug rather than the story id, because every other digest read is keyed
-- by slug and a caller assembling a page should not have to hold two kinds of
-- handle for the same story.
data DigestArticleRow = DigestArticleRow
  { darStorySlug :: Text
  , darArticle   :: ArticleRow
  } deriving (Eq, Show)

-- | The entries of one digest, in display order, joined to their story slugs.
digestEntries :: Store -> Int64 -> IO [DigestEntryRow]
digestEntries (Store conn) did = do
  rows <- query conn
    "SELECT s.slug, de.kind, de.delta_md, de.position \
    \ FROM digest_entry de JOIN story s ON s.id = de.story_id \
    \ WHERE de.digest_id = ? ORDER BY de.position"
    (Only did)
  pure [ DigestEntryRow slug kind delta pos | (slug, kind, delta, pos) <- rows ]

-- | The coverage one digest gathered: every article @digest_article@ points at,
-- newest first.
--
-- Ordered by publication rather than by the story order, because this answers
-- "what came in today" — the per-story reading order is what 'digestEntries'
-- already provides, and sorting the coverage the same way would just be that
-- list again with the URLs attached. Undated coverage sorts last rather than
-- first, so a feed that omits dates cannot lead the day.
digestArticles :: Store -> Int64 -> IO [DigestArticleRow]
digestArticles (Store conn) did = do
  rows <- query conn
    "SELECT s.slug, a.id, a.url_canonical, a.url_original, a.title, a.outlet, \
    \       a.source_id, a.guid, a.published_at, a.fetched_at, a.excerpt \
    \  FROM digest_article da \
    \  JOIN article a ON a.id = da.article_id \
    \  JOIN story s ON s.id = a.story_id \
    \ WHERE da.digest_id = ? \
    \ ORDER BY a.published_at IS NULL, a.published_at DESC, a.id"
    (Only did)
  pure [ DigestArticleRow slug (articleFromTuple rest)
       | (Only slug :. rest) <- rows ]

--------------------------------------------------------------------------------
-- Counting
--------------------------------------------------------------------------------

storyCount :: Store -> IO Int64
storyCount (Store conn) = do
  [Only n] <- query_ conn "SELECT COUNT(*) FROM story"
  pure n

articleCount :: Store -> IO Int64
articleCount (Store conn) = do
  [Only n] <- query_ conn "SELECT COUNT(*) FROM article"
  pure n
