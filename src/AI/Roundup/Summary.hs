{-# LANGUAGE OverloadedStrings #-}

-- | Turning a pile of new items into a roundup.
--
-- This is the whole reason the project takes a dependency on Conspire rather
-- than an HTTP client and a JSON encoder: the source is looked up by name from
-- the router at run time, so which model writes the roundup is a configuration
-- question rather than a code change.
--
-- One 'chat' call, no tool loop and no actor. The model is given everything it
-- needs in the prompt and has nothing to look up, so a loop would only add
-- turns for it to decline to use. When the roundup wants to follow links and
-- read the articles, this becomes an 'Actor' with a fetch tool and that is the
-- point to reach for @Conspire.Actor@.

module AI.Roundup.Summary
  ( summariseItems
  , renderItem
  ) where

import AI.Roundup.Store (StoredItem (..))
import Conspire
import Conspire.Provider (filterToolChats)
import Conspire.Provider.Kudzu (KudzuProvider, fetchKudzuSources)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Ask the named source for a roundup of these items.
--
-- Takes the provider rather than a source so the model list is fetched inside
-- the same 'Conspiracy' run as the request. Fetching it once at startup would
-- be faster, and is the right change once this runs as a daemon rather than as
-- a command.
summariseItems :: KudzuProvider -> Text -> [StoredItem] -> Conspiracy Text
summariseItems _ _ [] = pure "Nothing new."
summariseItems provider sourceName items = do
  sources <- filterToolChats <$> fetchKudzuSources provider
  case Map.lookup sourceName sources of
    Nothing -> throwError . ConspiracyError $
      "no source named " <> sourceName <> " (have: "
        <> T.intercalate ", " (Map.keys sources) <> ")"
    Just source -> do
      -- The instructions go in the system prompt rather than as a leading
      -- message: they are the same on every call, which is what a provider's
      -- prompt cache keys on, and the items are the only part that varies.
      reply <- chat source roundupPrompt
        [ userText (T.intercalate "\n\n" (map renderItem items)) ]
      pure (completionContent reply)

-- | What the model is asked to do.
--
-- Deliberately narrow. The failure mode of a summarisation prompt is a model
-- that pads — restating the feed's own blurb back at you with more adjectives —
-- so the instruction is mostly about what to leave out.
roundupPrompt :: Text
roundupPrompt = T.unlines
  [ "You are writing a short roundup of new items from the user's feeds."
  , ""
  , "Group related items together under a heading. Within a group, say what"
  , "actually happened in one or two sentences per item, and keep the link."
  , "Drop items that are pure marketing or that duplicate another item."
  , ""
  , "Do not pad. If only one thing is worth reporting, report one thing. If"
  , "nothing is, say so. Never invent detail that is not in the item text —"
  , "you are working from feed summaries, which are often truncated, and"
  , "guessing at the rest of an article is worse than omitting it."
  ]

-- | One item as the model sees it.
--
-- Plain labelled lines rather than JSON: the model reads this, nothing parses
-- it, and JSON would spend tokens on syntax. The link is included because the
-- roundup is expected to carry it through to the reader.
renderItem :: StoredItem -> Text
renderItem item = T.intercalate "\n"
  [ "TITLE: " <> storedTitle item
  , "SOURCE: " <> storedSource item
  , "LINK: " <> fromMaybe "(none)" (storedLink item)
  , "DATE: " <> maybe "(unknown)" (T.pack . show) (storedDate item)
  , "SUMMARY: " <> fromMaybe "(none)" (storedSummary item)
  ]
