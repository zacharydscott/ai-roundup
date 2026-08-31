{-# LANGUAGE OverloadedStrings #-}

-- | Fetching and parsing feeds.
--
-- The @feed@ package covers RSS 0.9x, RSS 1.0, RSS 2.0 and Atom behind one
-- parser and one query API, which is the whole reason to take the dependency:
-- the alternative is branching on the document type at every call site.
--
-- Everything it returns is optional, because every field it exposes is optional
-- in at least one of those formats. 'FeedItem' resolves that once, here, so the
-- rest of the program can assume an item has an identity and a title.
--
-- Transport only. Turning a 'FeedItem' into an "AI.Roundup.Ingest" candidate
-- happens there, not here, so this module stays exercisable against a fixture
-- and knows nothing about the corpus.

module AI.Roundup.Ingest.Feed
  ( FeedItem (..)
  , fetchFeed
  , parseItems
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client
       ( Manager, httpLbs, parseRequest, requestHeaders, responseBody
       , responseStatus )
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query
import Text.Feed.Types (Item)

-- | One entry, with the optionality already resolved.
data FeedItem = FeedItem
  { itemGuid    :: Text
    -- ^ Stable identity, used to decide whether an item has been seen before.
    -- Falls back to the link and then the title, because a surprising number of
    -- feeds omit @guid@ entirely — see 'itemIdentity'.
  , itemTitle   :: Text
  , itemLink    :: Maybe Text
  , itemSummary :: Maybe Text
  , itemDate    :: Maybe UTCTime
  , itemSource  :: Text
    -- ^ The feed URL this came from, not anything inside the document. Feeds
    -- lie about their own address often enough that the URL we actually
    -- fetched is the only reliable answer.
  } deriving (Eq, Show)

-- | Fetch a feed and parse it.
--
-- Returns 'Left' rather than throwing: one broken feed in a list of twenty
-- should cost that feed, not the run. Network exceptions are the caller's to
-- catch — 'httpLbs' throws, and catching here would mean guessing at a policy
-- for a program that does not exist yet.
fetchFeed :: Manager -> Text -> IO (Either Text [FeedItem])
fetchFeed manager url = do
  base <- parseRequest (T.unpack url)
  let request = base { requestHeaders = userAgent : requestHeaders base }
  response <- httpLbs request manager
  let code = statusCode (responseStatus response)
  pure $ if code >= 200 && code < 300
    then parseItems url (responseBody response)
    else Left $ "HTTP " <> T.pack (show code) <> " from " <> url

-- | Identify ourselves.
--
-- Not politeness: @http-client@ sends no @User-Agent@ at all by default, and
-- several publishers reject that outright rather than serve it — Ars Technica
-- answers 403 to a headerless request and 200 to the identical one with this
-- set. A named agent also gives an operator whose feed we are hammering
-- somebody to complain to, which is the reason it names the project rather
-- than impersonating a browser.
userAgent :: Header
userAgent = ("User-Agent", "ai-roundup/0.1 (+https://github.com/zacharydscott/ai-roundup)")

-- | Parse a feed document that has already been fetched.
--
-- Split from 'fetchFeed' so it can be tested against a fixture without a
-- network round trip.
parseItems :: Text -> BL.ByteString -> Either Text [FeedItem]
parseItems url body = case parseFeedSource body of
  Nothing   -> Left $ "could not parse a feed at " <> url
  Just feed -> Right (map (toItem url) (getFeedItems feed))

toItem :: Text -> Item -> FeedItem
toItem url item = FeedItem
  { itemGuid    = itemIdentity item
  , itemTitle   = fromMaybe "(untitled)" (getItemTitle item)
  , itemLink    = getItemLink item
  , itemSummary = getItemSummary item
  , itemDate    = publishDate item
  , itemSource  = url
  }

-- | The most stable identifier the item offers.
--
-- @guid@ is the field meant for this, but plenty of feeds omit it, so this
-- degrades to the link and finally to the title. The title is a poor identity —
-- an edited headline reads as a new item — but it is better than dropping the
-- entry, and it is the last resort rather than the common case.
itemIdentity :: Item -> Text
itemIdentity item = case getItemId item of
  Just (_, gid) -> gid
  Nothing       -> fromMaybe (fromMaybe "" (getItemTitle item)) (getItemLink item)

-- | Publication time, with @feed@'s double 'Maybe' flattened.
--
-- The outer 'Maybe' is "no date field", the inner is "date field that did not
-- parse". Nothing downstream distinguishes them: either way the item has no
-- usable timestamp and gets ordered by insertion instead.
publishDate :: Item -> Maybe UTCTime
publishDate item = case getItemPublishDate item of
  Just (Just t) -> Just t
  _             -> Nothing
