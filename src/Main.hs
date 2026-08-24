{-# LANGUAGE OverloadedStrings #-}

-- | Entry point.
--
-- One command, three steps: poll the feeds, store what is new, summarise what
-- has not been summarised yet. Every step is idempotent, so running it twice in
-- a row produces a roundup the first time and "Nothing new." the second — which
-- is what makes it safe to put on a timer.

module Main (main) where

import AI.Roundup.Feed (FeedItem (..), fetchFeed)
import AI.Roundup.Store
import AI.Roundup.Summary (summariseItems)
import Conspire (newManager, runConspiracyWith)
import Conspire.Monad (ConspiracyError (..))
import Conspire.Provider.Kudzu (mkKudzuProvider)
import Control.Exception (SomeException, try)
import Control.Monad (forM, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optFeeds  :: [Text]
  , optStore  :: FilePath
  , optSource :: Text
  , optLimit  :: Int
  , optRouter :: Text
  } deriving (Eq, Show)

parser :: Parser Options
parser = Options
  <$> some (strOption
        (long "feed" <> metavar "URL"
        <> help "Feed to poll; repeatable"))
  <*> strOption
        (long "store" <> metavar "PATH" <> value "roundup.sqlite" <> showDefault
        <> help "SQLite database recording which items have been seen")
  <*> strOption
        (long "source" <> metavar "NAME" <> value "local" <> showDefault
        <> help "Model source to ask for the roundup, by router name")
  <*> option auto
        (long "limit" <> metavar "N" <> value 40 <> showDefault
        <> help "Most items to put in one roundup")
  <*> strOption
        (long "router" <> metavar "URL"
        <> value "https://gx10-da00.local:2620" <> showDefault
        <> help "Kudzu router base URL")

main :: IO ()
main = do
  opts <- execParser $ info (parser <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Poll feeds and summarise what is new"
    , header "ai-roundup — a feed-watching agent built on Conspire"
    ]

  -- Read rather than required up front: polling and storing work without a
  -- key, and only the summarising step needs one. Failing at startup would
  -- refuse to do the half of the job that is still possible offline.
  mKey <- lookupEnv "KUDZU_ROUTER_KEY"

  httpManager <- TLS.newTlsManager

  withStore (optStore opts) $ \store -> do
    fresh <- concat <$> forM (optFeeds opts) (pollFeed httpManager store)
    putStrLn $ show (length fresh) <> " new item(s) across "
            <> show (length (optFeeds opts)) <> " feed(s)"

    pending <- pendingItems store (optLimit opts)
    unless (null pending) $ case mKey of
      Nothing -> do
        hPutStrLn stderr $
          "ai-roundup: " <> show (length pending) <> " item(s) waiting, but "
          <> "KUDZU_ROUTER_KEY is unset, so nothing was summarised"
        exitFailure
      Just key -> do
        -- Timeout is generous because a roundup over forty items is a long
        -- single completion; the concurrency limit is 1 because there is
        -- exactly one request in flight.
        conspiracyManager <- newManager 120000 1
        let provider = mkKudzuProvider (optRouter opts) (T.pack key)
        (result, _log) <- runConspiracyWith conspiracyManager $
          summariseItems provider (optSource opts) pending
        case result of
          Left err -> do
            hPutStrLn stderr $ "ai-roundup: " <> T.unpack (conspiracyError err)
            exitFailure
          Right roundup -> do
            TIO.putStrLn ""
            TIO.putStrLn roundup
            -- Only after the roundup is in hand: a run that dies mid-request
            -- should repeat the work rather than drop the items silently.
            markSummarised store (map storedId pending)

-- | Fetch one feed and store whatever is new in it.
--
-- A feed that fails is reported and skipped. One unreachable host in a list of
-- twenty should not cost the other nineteen, and the exception has to be caught
-- here because 'fetchFeed' leaves network errors to the caller.
pollFeed :: HTTP.Manager -> Store -> Text -> IO [FeedItem]
pollFeed manager store url = do
  outcome <- try (fetchFeed manager url)
  case outcome of
    Left err -> do
      hPutStrLn stderr $
        "ai-roundup: " <> T.unpack url <> ": " <> show (err :: SomeException)
      pure []
    Right (Left err) -> do
      hPutStrLn stderr $ "ai-roundup: " <> T.unpack err
      pure []
    Right (Right items) -> insertNew store items
