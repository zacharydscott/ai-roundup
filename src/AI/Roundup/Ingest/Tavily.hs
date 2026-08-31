{-# LANGUAGE OverloadedStrings #-}

-- | Tavily search, over MCP.
--
-- Tavily is reached as an MCP server rather than through its REST API because
-- conspire already speaks MCP and already knows how to authenticate against it
-- (@tavilyApiKey@ as a query parameter, see 'tavilyClient'); a second HTTP
-- client here would duplicate retry, session and error handling for one call.
--
-- This is a raw @tools\/call@ rather than the tool map conspire builds for
-- models ('Conspire.MCP.getToolSource'), because nothing here is a model
-- deciding to search: the queries come from @sources.yaml@ and the results are
-- data for the pipeline. Going through an agent would spend tokens to make a
-- call we can make ourselves.
--
-- Transport only, like "AI.Roundup.Ingest.Feed": a 'TavilyResult' becomes a
-- candidate in "AI.Roundup.Ingest".

module AI.Roundup.Ingest.Tavily
  ( TavilyResult (..)
  , tavilyClient
  , tavilyClientFromEnv
  , tavilySearch
  ) where

import AI.Roundup.Config (TavilySource (..))
import Conspire.MCP (Client, newClient, sendRequest)
import Conspire.MCP.Auth (AuthConfig (QueryParamAuth))
import Conspire.MCP.Types
  ( Response (..), ServerConfig (..), ToolCallParams (..), ToolResult (..)
  , toolResultText )
import Conspire.Monad (Conspiracy, ConspiracyError (..))
import Control.Monad.Except (throwError)
import Data.Aeson
  ( FromJSON (..), Value, eitherDecodeStrict', object, withObject
  , (.:), (.:?), (.!=), (.=) )
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)

-- | One search hit.
--
-- @published_date@ is parsed but is almost always absent: the Tavily MCP
-- server pins @topic@ to @general@, and only the @news@ topic carries dates.
-- A search result therefore usually reaches the resolver with no publication
-- date, which matters because story dates are publication dates — see the note
-- on ordering in "AI.Roundup.Ingest".
data TavilyResult = TavilyResult
  { trUrl           :: Text
  , trTitle         :: Text
  , trContent       :: Text   -- ^ Tavily's extracted snippet, already plain text.
  , trScore         :: Maybe Double
  , trPublishedDate :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON TavilyResult where
  parseJSON = withObject "tavily result" $ \o -> TavilyResult
    <$> o .:  "url"
    <*> o .:  "title"
    <*> o .:? "content" .!= ""
    <*> o .:? "score"
    <*> o .:? "published_date"

-- | An MCP client for Tavily's hosted server.
--
-- Not opened here as a bracket: the client is created once per run and shared
-- by every query, and "AI.Roundup.Ingest" is handed a client rather than a key
-- so a run with no @TAVILY_API_KEY@ degrades to feeds only instead of failing.
tavilyClient :: Text -> IO (Either Text Client)
tavilyClient apiKey = newClient RemoteServer
  { remoteServerUrl  = "https://mcp.tavily.com/mcp"
  , remoteServerName = "tavily"
  , remoteServerAuth = QueryParamAuth "tavilyApiKey" (TE.encodeUtf8 apiKey)
  }

-- | The same, keyed from the environment. 'Nothing' means no key was set,
-- which is a degraded run rather than an error.
tavilyClientFromEnv :: IO (Maybe (Either Text Client))
tavilyClientFromEnv = do
  mKey <- lookupEnv "TAVILY_API_KEY"
  case mKey of
    Nothing  -> pure Nothing
    Just ""  -> pure Nothing
    Just key -> Just <$> tavilyClient (T.pack key)

-- | Run one configured search.
--
-- Optional arguments are omitted rather than defaulted, so the server's own
-- defaults apply and this module does not have to track them.
tavilySearch :: Client -> TavilySource -> Conspiracy [TavilyResult]
tavilySearch client src = do
  resp <- sendRequest client "tools/call"
            (Just (ToolCallParams "tavily_search" (searchArgs src)))
  raw <- case resp of
    Success val         -> pure val
    ServerError _ msg _ -> failWith msg
    ConnectionError msg -> failWith msg
  result <- either (failWith . T.pack) pure (parseEither parseJSON raw)
  if toolResultIsError result
    -- An MCP tool error arrives as a successful JSON-RPC response carrying
    -- isError, so it has to be checked separately or a rejected query looks
    -- like zero results.
    then failWith (toolResultText result)
    else parseResults (toolResultText result)
  where
    failWith :: Text -> Conspiracy a
    failWith msg = throwError . ConspiracyError $
      "tavily search " <> tsId src <> ": " <> msg

    -- The tool returns its payload as a JSON document inside a text content
    -- block, so the JSON is parsed twice: once out of the MCP envelope, once
    -- out of the text.
    parseResults txt =
      case eitherDecodeStrict' (TE.encodeUtf8 txt) of
        Left err          -> failWith ("unparseable payload: " <> T.pack err)
        Right (Payload r) -> pure r

searchArgs :: TavilySource -> Value
searchArgs src = object $
  [ "query"       .= tsQuery src
  , "max_results" .= tsMaxResults src
  ]
  <> [ "time_range"      .= t | Just t <- [tsTimeRange src] ]
  <> [ "search_depth"    .= d | Just d <- [tsSearchDepth src] ]
  <> [ "include_domains" .= tsIncludeDomains src | not (null (tsIncludeDomains src)) ]
  <> [ "exclude_domains" .= tsExcludeDomains src | not (null (tsExcludeDomains src)) ]

-- | The @results@ array of a @tavily_search@ payload. A newtype so the rest of
-- the response (the answer, images, follow-up questions — none of which this
-- program wants) can be ignored by the parser rather than by every caller.
newtype Payload = Payload [TavilyResult]

instance FromJSON Payload where
  parseJSON = withObject "tavily payload" $ \o -> Payload <$> o .: "results"
