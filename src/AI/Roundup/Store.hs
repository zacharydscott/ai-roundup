{-# LANGUAGE OverloadedStrings #-}

-- | The SQLite store.
--
-- Two jobs: remember which items have already been seen, and remember which of
-- those have already been summarised. Both are the same shape of problem — the
-- program is restartable and polls repeatedly, so anything it has already done
-- has to survive the process exiting.
--
-- The dedup key is @(source, guid)@ rather than @guid@ alone. Feed guids are
-- only promised to be unique within their own feed, and two feeds carrying the
-- same syndicated article legitimately use the same one; scoping to the source
-- keeps a shared guid from silently hiding one of them.

module AI.Roundup.Store
  ( Store
  , withStore
  , insertNew
  , pendingItems
  , markSummarised
  , StoredItem (..)
  ) where

import AI.Roundup.Feed (FeedItem (..))
import Control.Monad (forM)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple

-- | An open connection. A newtype so callers cannot reach past the functions
-- here and run ad-hoc SQL against a schema this module owns.
newtype Store = Store Connection

-- | An item as it came back out of the database, carrying its row id.
data StoredItem = StoredItem
  { storedId      :: Int64
  , storedTitle   :: Text
  , storedLink    :: Maybe Text
  , storedSummary :: Maybe Text
  , storedDate    :: Maybe UTCTime
  , storedSource  :: Text
  } deriving (Eq, Show)

instance FromRow StoredItem where
  fromRow = StoredItem <$> field <*> field <*> field <*> field <*> field <*> field

-- | Open the store at a path, create the schema if absent, and close it after.
--
-- The schema is applied on every open rather than guarded by a version check:
-- at one table with @IF NOT EXISTS@ that is honest, and a migration system
-- before there is anything to migrate would be decoration.
withStore :: FilePath -> (Store -> IO a) -> IO a
withStore path action = withConnection path $ \conn -> do
  -- WAL so a long summarisation run does not block a concurrent poll. Harmless
  -- for a single process, and the failure it prevents is confusing to diagnose.
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS items \
    \( id           INTEGER PRIMARY KEY \
    \, source       TEXT NOT NULL \
    \, guid         TEXT NOT NULL \
    \, title        TEXT NOT NULL \
    \, link         TEXT \
    \, summary      TEXT \
    \, published    TIMESTAMP \
    \, first_seen   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \, summarised_at TIMESTAMP \
    \, UNIQUE (source, guid) )"
  -- Covers the pending query, which is the only one on the hot path.
  execute_ conn
    "CREATE INDEX IF NOT EXISTS items_pending \
    \ON items (summarised_at, published)"
  action (Store conn)

-- | Insert the items not already present, returning the ones actually stored.
--
-- @ON CONFLICT DO NOTHING@ does the deduplication in the database rather than
-- by reading the whole table into memory first, which also makes it correct if
-- a second poller is running.
--
-- Returns the new items so the caller can report "3 new" without a second
-- query. 'changes' is not usable for that here: it reports the last statement's
-- row count, and a batch would only tell you about the final insert.
insertNew :: Store -> [FeedItem] -> IO [FeedItem]
insertNew (Store conn) items = withTransaction conn $
  fmap catMaybes . forM items $ \item -> do
    execute conn
      "INSERT INTO items (source, guid, title, link, summary, published) \
      \VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (source, guid) DO NOTHING"
      ( itemSource item, itemGuid item, itemTitle item
      , itemLink item, itemSummary item, itemDate item )
    inserted <- changes conn
    pure $ if inserted > 0 then Just item else Nothing

-- | Items that have not been summarised yet, oldest first.
--
-- Oldest first because a roundup reads as a narrative; newest first would
-- deliver the story backwards. Items with no publication date sort last rather
-- than first, since a missing date is more often a broken feed than a very old
-- item.
pendingItems :: Store -> Int -> IO [StoredItem]
pendingItems (Store conn) limit = query conn
  "SELECT id, title, link, summary, published, source FROM items \
  \WHERE summarised_at IS NULL \
  \ORDER BY published IS NULL, published ASC, id ASC LIMIT ?"
  (Only limit)

-- | Mark items as included in a roundup.
--
-- Called after the summary is in hand, not before: a run that dies mid-request
-- should repeat the work, and repeating is cheap while silently dropping an
-- item from every future roundup is not.
markSummarised :: Store -> [Int64] -> IO ()
markSummarised _ [] = pure ()
markSummarised (Store conn) ids = withTransaction conn $
  mapM_ (\i -> execute conn
          "UPDATE items SET summarised_at = CURRENT_TIMESTAMP WHERE id = ?"
          (Only i))
        ids
