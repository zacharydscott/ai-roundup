{-# LANGUAGE OverloadedStrings #-}

-- | Wave 3: the corpus as pages.
--
-- One page per digest under @pages\/roundups\/<date>.html@, plus an archive
-- index at @pages\/index.html@. @pages\/@ is the directory Cloudflare Pages is
-- pointed at (see @wrangler.toml@), so what this writes is what gets served,
-- with no build step in between.
--
-- __Everything is inlined.__ No external stylesheet, no font CDN, no script.
-- The pages are read by people skimming a briefing on a phone on Monday
-- morning, and a blocking request to a font host is the difference between that
-- working and not. It also means a page is a complete artefact: saved, mailed
-- or opened from disk, it still looks like itself.
--
-- __The ranking is the order and the numeral, not the type size.__
-- @digest_entry.position@ is the output of a model that compared every story of
-- the day against every other, and "AI.Roundup.Compose"'s rank prompt promises
-- the first five are shown on their own. Both facts are carried by the numeral
-- in the margin and by the break after the fifth story. Every story is then set
-- at the same size: a ladder of headline sizes said the same thing a second
-- time, and it made the tail of a long day look like an afterthought when it is
-- just the tail.
--
-- __Prose is set in serif, machine facts in mono.__ The write-ups and deltas
-- are generated prose; the dates, outlets and source ids are database columns.
-- The page never mixes the two voices, so a reader can tell at a glance which
-- part of a page a model wrote and which part is provenance.

module AI.Roundup.Site
  ( writeRoundupPages
  , writeRoundupPage
  , defaultPagesDir
    -- * Rendering (exposed for inspection and tests)
  , roundupHtml
  , indexHtml
  ) where

import AI.Roundup.Store
       ( ArticleRow (..), DigestArticleRow (..), DigestEntryRow (..)
       , DigestRow (..), Store, StoryDetailRow (..), digestArticles
       , digestEntries, digestForDate, digests, storyDetailForSlug )

import Data.Char (isSpace)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))

-- | Beside @data\/@ rather than inside it: @data\/@ is the run's working set
-- and is git-ignored in part, while this is the published artefact and is
-- committed, because pushing the repo is what deploys it.
defaultPagesDir :: FilePath
defaultPagesDir = "pages"

--------------------------------------------------------------------------------
-- The shape a page is built from
--------------------------------------------------------------------------------

-- | One day, assembled. Flattened out of four queries so that rendering is a
-- pure function of this and nothing else — the HTML below never touches the
-- store, which is what makes it testable without a database.
data PageDigest = PageDigest
  { pdDate    :: Text
  , pdIntro   :: Maybe Text
  , pdStories :: [PageStory]
  }

data PageStory = PageStory
  { psRank     :: Int
  , psTitle    :: Text
  , psSlug     :: Text
  , psCategory :: Text
  , psKind     :: Text
  , psDelta    :: Maybe Text
  , psCoverage :: [ArticleRow]
  }

--------------------------------------------------------------------------------
-- Writing
--------------------------------------------------------------------------------

-- | Rebuild every page in the corpus. What @site build@ runs.
writeRoundupPages :: Store -> FilePath -> IO [FilePath]
writeRoundupPages store out = do
  days <- digests store
  pages <- mapM (loadDigest store) days
  paths <- mapM (writeOne out) pages
  writeIndex out pages
  pure (paths ++ [out </> "index.html"])

-- | Rebuild one day, and the index that has to mention it.
--
-- The index is rewritten rather than patched because it is a listing of every
-- day: a run that appended to it would drift from the corpus the first time a
-- digest was edited or a day rebuilt, and the file is tens of lines.
writeRoundupPage :: Store -> FilePath -> Text -> IO (Maybe FilePath)
writeRoundupPage store out date = do
  mDigest <- digestForDate store date
  case mDigest of
    Nothing -> pure Nothing
    Just row -> do
      page <- loadDigest store row
      path <- writeOne out page
      days <- digests store >>= mapM (loadDigest store)
      writeIndex out days
      pure (Just path)

writeOne :: FilePath -> PageDigest -> IO FilePath
writeOne out page = do
  createDirectoryIfMissing True (out </> "roundups")
  let path = out </> "roundups" </> T.unpack (pdDate page) <.> "html"
  TIO.writeFile path (roundupHtml page)
  pure path

-- | The archive, and the page Cloudflare serves for a path that is not a day.
--
-- Both are written together because both are functions of the same list, and a
-- 404 that did not know which days exist would be a dead end rather than a
-- redirect a reader can act on. @wrangler.toml@ names @404.html@ directly, so
-- this file existing is part of that configuration being true.
writeIndex :: FilePath -> [PageDigest] -> IO ()
writeIndex out pages = do
  createDirectoryIfMissing True out
  TIO.writeFile (out </> "index.html") (indexHtml pages)
  TIO.writeFile (out </> "404.html") (notFoundHtml pages)

-- | Four queries into one value.
--
-- The coverage comes from @digest_article@ and is bucketed by story slug, which
-- is the whole reason that table exists: without it the only way to say which
-- articles belong to a day is @fetched_at@, and a rebuilt corpus stamps every
-- row with the day it was rebuilt.
loadDigest :: Store -> DigestRow -> IO PageDigest
loadDigest store row = do
  entries <- digestEntries store (digestId row)
  arts    <- digestArticles store (digestId row)
  ranked  <- mapM (loadStory store arts) (zip [1 ..] entries)
  pure PageDigest
    { pdDate    = digestDate row
    , pdIntro   = digestIntro row
    , pdStories = catMaybes ranked
    }

-- | The rank is the entry's place in the ordered list, not
-- @digest_entry.position@ itself. They agree today, but position is written by
-- whatever composed the day and nothing constrains it to be dense — a hand-
-- edited change set that skips a number should still produce 1, 2, 3 on the
-- page rather than a gap the reader has to explain to themselves.
loadStory
  :: Store -> [DigestArticleRow] -> (Int, DigestEntryRow) -> IO (Maybe PageStory)
loadStory store arts (rank, entry) = do
  mDetail <- storyDetailForSlug store (derStorySlug entry)
  pure $ fmap (\d -> PageStory
    { psRank     = rank
    , psTitle    = sdrTitle d
    , psSlug     = sdrSlug d
    , psCategory = sdrCategory d
    , psKind     = derKind entry
    , psDelta    = derDelta entry
    , psCoverage = [ darArticle a | a <- arts
                   , darStorySlug a == derStorySlug entry ]
    }) mDetail

--------------------------------------------------------------------------------
-- One day's page
--------------------------------------------------------------------------------

roundupHtml :: PageDigest -> Text
roundupHtml page = document title "day" body
  where
    title = "AI Roundup — " <> longDate (pdDate page)
    body = T.concat
      [ "<header class=\"masthead\">"
      , "<p class=\"eyebrow\"><a href=\"../index.html\">AI Roundup</a>"
      , "<span class=\"stamp\">", esc (pdDate page), "</span></p>"
      , "<h1>", esc (longDate (pdDate page)), "</h1>"
      , "<p class=\"tally\">", esc (tally page), "</p>"
      , "</header>"
      , maybe "" (\i -> "<div class=\"intro\">" <> blocks i <> "</div>") (pdIntro page)
      , stories (pdStories page)
      , footer
      ]

-- | The day at a glance, in the mono voice: counts are facts about the corpus.
tally :: PageDigest -> Text
tally page = T.intercalate " · " (filter (not . T.null) [storyPart, sourcePart])
  where
    n = length (pdStories page)
    outlets = distinct (mapMaybe artOutlet (concatMap psCoverage (pdStories page)))
    storyPart = plural n "story" "stories"
    sourcePart | null outlets = ""
               | otherwise    = plural (length outlets) "outlet" "outlets"

-- | The ranked list, broken where the ranking promises it will be.
--
-- An empty day is not an error and does not get an apology: the sources were
-- read and had nothing, which is a fact about the day worth stating plainly.
stories :: [PageStory] -> Text
stories [] = "<p class=\"empty\">No stories today. The sources were checked and \
             \had nothing that changes what a company can buy, run or owe.</p>"
stories ss = T.concat
  [ "<ol class=\"stories\">"
  , T.concat (map story lead)
  , if null tail' then "" else
      "<li class=\"break\" aria-hidden=\"true\"><span>Also today</span></li>"
        <> T.concat (map story tail')
  , "</ol>"
  ]
  where (lead, tail') = splitAt 5 ss

story :: PageStory -> Text
story s = T.concat
  [ "<li class=\"story\">"
  , "<span class=\"rank\" aria-hidden=\"true\">", esc (tshow (psRank s)), "</span>"
  , "<div class=\"body\">"
  , "<h2>", esc (psTitle s), "</h2>"
  , "<p class=\"flags\">"
  , "<span class=\"flag ", categoryClass (psCategory s), "\">"
  , esc (categoryLabel (psCategory s)), "</span>"
  , if psKind s == "update"
      then "<span class=\"flag continuing\">Continuing</span>"
      else ""
  , "</p>"
  , maybe "" (\d -> "<div class=\"delta\">" <> blocks d <> "</div>") (psDelta s)
  , coverage (psCoverage s)
  , "</div></li>"
  ]

-- | Provenance, in the mono voice. Outlet and date before the link text,
-- because a reader deciding whether to click is deciding on the outlet.
coverage :: [ArticleRow] -> Text
coverage [] = ""
coverage arts = T.concat
  [ "<ul class=\"coverage\">"
  , T.concat
      [ T.concat
          [ "<li>"
          , link (artUrlCanonical a)
              (fromMaybe (artUrlCanonical a) (artTitle a))
          , "<span class=\"source\">"
          , esc (fromMaybe "unattributed" (artOutlet a))
          , maybe "" (\d -> " · " <> esc (T.take 10 d)) (artPublishedAt a)
          , "</span></li>"
          ]
      | a <- arts
      ]
  , "</ul>"
  ]

--------------------------------------------------------------------------------
-- The archive index
--------------------------------------------------------------------------------

indexHtml :: [PageDigest] -> Text
indexHtml pages = document "AI Roundup" "index" body
  where
    body = T.concat
      [ "<header class=\"masthead\">"
      , "<p class=\"eyebrow\">AI Roundup"
      , "<span class=\"stamp\">", esc (plural (length pages) "day" "days")
      , "</span></p>"
      , "<h1>What changed, and what it costs you.</h1>"
      , "<p class=\"tally\">A daily read of AI news for the people who buy, \
        \deploy and answer for it.</p>"
      , "</header>"
      , if null pages
          then "<p class=\"empty\">No roundups yet. The first run writes one.</p>"
          else "<ul class=\"archive\">" <> T.concat (map archiveRow pages) <> "</ul>"
      , footer
      ]

archiveRow :: PageDigest -> Text
archiveRow page = T.concat
  [ "<li><a href=\"roundups/", esc (pdDate page), ".html\">"
  , "<span class=\"stamp\">", esc (pdDate page), "</span>"
  , "<span class=\"headline\">", esc (leadTitle page), "</span>"
  , "<span class=\"count\">", esc (tally page), "</span>"
  , "</a></li>"
  ]

-- | The index lists days by their lead story, which is the ranking earning its
-- keep a second time: the top-ranked story is the honest one-line answer to
-- "what happened that day".
leadTitle :: PageDigest -> Text
leadTitle page = case pdStories page of
  (s : _) -> psTitle s
  []      -> "Nothing filed"

-- | Served for any path that is not a day. Names the most recent days rather
-- than apologising, because a reader who lands here guessed at a date and the
-- dates that exist are the useful answer.
notFoundHtml :: [PageDigest] -> Text
notFoundHtml pages = document "Not a day we filed — AI Roundup" "index" body
  where
    recent = take 5 pages
    body = T.concat
      [ "<header class=\"masthead\">"
      , "<p class=\"eyebrow\"><a href=\"/\">AI Roundup</a>"
      , "<span class=\"stamp\">404</span></p>"
      , "<h1>No roundup at that address.</h1>"
      , "<p class=\"tally\">Roundups are filed one per day, at "
      , "/roundups/YYYY-MM-DD.</p>"
      , "</header>"
      , if null recent
          then "<p class=\"empty\">Nothing has been filed yet.</p>"
          else "<ul class=\"archive\">"
                 <> T.concat (map archiveRowAbsolute recent) <> "</ul>"
      , footer
      ]

-- | The archive rows again, with root-relative hrefs. The 404 is served from
-- any depth — @\/roundups\/nonsense@ as readily as @\/nonsense@ — so a relative
-- link out of it would resolve differently depending on what the reader
-- mistyped.
archiveRowAbsolute :: PageDigest -> Text
archiveRowAbsolute page = T.concat
  [ "<li><a href=\"/roundups/", esc (pdDate page), ".html\">"
  , "<span class=\"stamp\">", esc (pdDate page), "</span>"
  , "<span class=\"headline\">", esc (leadTitle page), "</span>"
  , "<span class=\"count\">", esc (tally page), "</span>"
  , "</a></li>"
  ]

--------------------------------------------------------------------------------
-- Chrome
--------------------------------------------------------------------------------

-- | The shell every page shares. @bodyClass@ distinguishes the archive from a
-- day without a second stylesheet.
document :: Text -> Text -> Text -> Text
document title bodyClass body = T.concat
  [ "<!doctype html>\n<html lang=\"en\">\n<head>\n"
  , "<meta charset=\"utf-8\">\n"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
  , "<title>", esc title, "</title>\n"
  , "<meta name=\"color-scheme\" content=\"light dark\">\n"
  , "<style>\n", stylesheet, "</style>\n"
  , "</head>\n<body class=\"", esc bodyClass, "\">\n<main>\n"
  , body
  , "\n</main>\n</body>\n</html>\n"
  ]

footer :: Text
footer =
  "<footer><p>Compiled from feed and search coverage. Every claim links to the \
  \outlet that made it.</p></footer>"

-- | The whole visual system.
--
-- Two type voices and no more: a humanist serif for anything a model wrote, a
-- mono for anything the database knows. The rank numerals hang in the left
-- margin on a wide screen, where the column of figures reads as the ranking
-- without any story having to be set larger than its neighbours; below 46rem
-- they fold above the headline, because a hanging margin on a phone is just a
-- lost third of the measure.
stylesheet :: Text
stylesheet = T.unlines
  [ ":root {"
  , "  --paper: #EDEAE3;"
  , "  --raise: #F6F4EF;"
  , "  --ink: #1A1D21;"
  , "  --muted: #6B6A64;"
  , "  --rule: #CFCBC0;"
  , "  --accent: #1F4B99;"
  , "  --flag: #A8341F;"
  , "  --serif: 'Iowan Old Style', 'Palatino Linotype', Palatino, 'Book Antiqua', Georgia, serif;"
  , "  --mono: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace;"
  , "}"
  , "@media (prefers-color-scheme: dark) {"
  , "  :root {"
  , "    --paper: #15171B;"
  , "    --raise: #1D2026;"
  , "    --ink: #E6E3DB;"
  , "    --muted: #9A968C;"
  , "    --rule: #33373E;"
  , "    --accent: #8FB3F0;"
  , "    --flag: #E07A63;"
  , "  }"
  , "}"
  , "* { box-sizing: border-box; }"
  , "body {"
  , "  margin: 0; padding: 0 1.25rem 4rem;"
  , "  background: var(--paper); color: var(--ink);"
  , "  font-family: var(--serif);"
  , "  font-size: 17px; line-height: 1.55;"
  , "  -webkit-text-size-adjust: 100%;"
  , "}"
  , "main { max-width: 44rem; margin: 0 auto; }"
  , "a { color: var(--accent); text-underline-offset: 0.18em; }"
  , "a:focus-visible {"
  , "  outline: 2px solid var(--accent); outline-offset: 3px; border-radius: 2px;"
  , "}"
    -- Masthead. The eyebrow and the date stamp are the mono voice; the date
    -- itself is the one place the display face is allowed to be large.
  , ".masthead { padding: 3rem 0 1.75rem; border-bottom: 1px solid var(--rule); }"
  , ".eyebrow {"
  , "  display: flex; justify-content: space-between; align-items: baseline;"
  , "  gap: 1rem; margin: 0 0 1.5rem;"
  , "  font-family: var(--mono); font-size: 0.7rem;"
  , "  letter-spacing: 0.16em; text-transform: uppercase; color: var(--muted);"
  , "}"
  , ".eyebrow a { color: inherit; text-decoration: none; }"
  , ".eyebrow a:hover { color: var(--accent); }"
  , ".masthead h1 {"
  , "  margin: 0; font-weight: 600; letter-spacing: -0.015em;"
  , "  font-size: clamp(1.9rem, 6vw, 3rem); line-height: 1.08;"
  , "}"
  , ".tally {"
  , "  margin: 0.9rem 0 0; font-family: var(--mono);"
  , "  font-size: 0.78rem; color: var(--muted);"
  , "}"
  , ".intro {"
  , "  margin: 2rem 0 0; font-size: 1.12rem; line-height: 1.6;"
  , "  color: var(--ink);"
  , "}"
  , ".intro p:first-child { margin-top: 0; }"
    -- The ranked list. The numeral hangs; the body holds the measure.
  , ".stories { list-style: none; margin: 0; padding: 0; }"
  , ".story {"
  , "  display: grid; grid-template-columns: 4rem 1fr; gap: 0 1.25rem;"
  , "  padding: 2.25rem 0; border-bottom: 1px solid var(--rule);"
  , "}"
  , ".rank {"
  , "  font-family: var(--mono); color: var(--accent);"
  , "  font-size: 1rem; line-height: 1; font-variant-numeric: tabular-nums;"
  , "  text-align: right; padding-top: 0.45rem;"
  , "}"
  , ".story h2 {"
  , "  margin: 0; font-weight: 600; letter-spacing: -0.01em;"
  , "  font-size: clamp(1.15rem, 2.6vw, 1.35rem); line-height: 1.28;"
  , "  text-wrap: balance;"
  , "}"
    -- The tier break. A rule with a word on it, not a decoration: it marks
    -- where the ranking stops promising prominence.
  , ".break {"
  , "  display: flex; align-items: center; gap: 1rem;"
  , "  margin: 0.5rem 0 0; padding: 1.5rem 0 0;"
  , "  font-family: var(--mono); font-size: 0.68rem;"
  , "  letter-spacing: 0.2em; text-transform: uppercase; color: var(--muted);"
  , "}"
  , ".break::after {"
  , "  content: ''; flex: 1; height: 1px; background: var(--rule);"
  , "}"
  , ".flags { margin: 0.7rem 0 0; display: flex; flex-wrap: wrap; gap: 0.4rem; }"
  , ".flag {"
  , "  font-family: var(--mono); font-size: 0.64rem;"
  , "  letter-spacing: 0.12em; text-transform: uppercase;"
  , "  padding: 0.25rem 0.5rem; border: 1px solid var(--rule);"
  , "  color: var(--muted); background: var(--raise);"
  , "}"
    -- The two categories that cost money or invite a lawyer are the only ones
    -- that get ink. Everything else stays quiet, so the flag means something.
  , ".flag.money { color: var(--flag); border-color: var(--flag); }"
  , ".flag.continuing { color: var(--accent); border-color: var(--accent); }"
  , ".delta { margin: 0.85rem 0 0; }"
  , ".delta p { margin: 0 0 0.75rem; }"
  , ".delta p:last-child { margin-bottom: 0; }"
  , ".delta ul { margin: 0.5rem 0; padding-left: 1.1rem; }"
  , ".delta code {"
  , "  font-family: var(--mono); font-size: 0.88em;"
  , "  background: var(--raise); padding: 0.1em 0.3em;"
  , "}"
    -- Provenance.
  , ".coverage { list-style: none; margin: 1.1rem 0 0; padding: 0; }"
  , ".coverage li { margin: 0 0 0.5rem; line-height: 1.4; }"
  , ".coverage a { font-size: 0.95rem; text-decoration-thickness: 1px; }"
  , ".source {"
  , "  display: block; font-family: var(--mono); font-size: 0.7rem;"
  , "  letter-spacing: 0.06em; color: var(--muted); margin-top: 0.15rem;"
  , "}"
    -- Archive.
  , ".archive { list-style: none; margin: 0; padding: 0; }"
  , ".archive li { border-bottom: 1px solid var(--rule); }"
  , ".archive a {"
  , "  display: grid; grid-template-columns: 7rem 1fr; gap: 0.15rem 1.25rem;"
  , "  padding: 1.4rem 0; text-decoration: none; color: inherit;"
  , "}"
  , ".archive a:hover .headline { color: var(--accent); }"
  , ".archive .stamp {"
  , "  font-family: var(--mono); font-size: 0.78rem; color: var(--muted);"
  , "  padding-top: 0.25rem;"
  , "}"
  , ".archive .headline { font-size: 1.15rem; line-height: 1.3; }"
  , ".archive .count {"
  , "  grid-column: 2; font-family: var(--mono); font-size: 0.7rem;"
  , "  color: var(--muted);"
  , "}"
  , ".empty {"
  , "  margin: 3rem 0; padding: 1.5rem; background: var(--raise);"
  , "  border-left: 2px solid var(--rule); color: var(--muted);"
  , "}"
  , "footer {"
  , "  margin-top: 3rem; padding-top: 1.25rem; border-top: 1px solid var(--rule);"
  , "  font-family: var(--mono); font-size: 0.7rem; color: var(--muted);"
  , "}"
  , "footer p { margin: 0; }"
    -- A hanging numeral needs a margin to hang in. Below this width there
    -- isn't one, so it folds above the headline and shrinks out of the way.
  , "@media (max-width: 46rem) {"
  , "  .story { grid-template-columns: 1fr; gap: 0; }"
  , "  .rank { text-align: left; padding: 0 0 0.35rem; font-size: 0.85rem; }"
  , "  .archive a { grid-template-columns: 1fr; }"
  , "  .archive .count { grid-column: 1; }"
  , "}"
  , "@media (prefers-reduced-motion: reduce) {"
  , "  * { transition: none !important; animation: none !important; }"
  , "}"
  ]

--------------------------------------------------------------------------------
-- Categories
--------------------------------------------------------------------------------

-- | Category slugs are seeded by the schema but a change set may name one that
-- is not, so an unknown slug is titled rather than dropped.
categoryLabel :: Text -> Text
categoryLabel slug = case slug of
  "model"        -> "Model"
  "regulation"   -> "Regulation"
  "business"     -> "Business"
  "pricing"      -> "Pricing"
  "general_tech" -> "General"
  other          -> T.toTitle (T.replace "_" " " other)

-- | Only regulation and pricing take ink. They are the two categories the
-- ranking promotes hardest, and a flag that every story carries in colour is a
-- flag that says nothing.
categoryClass :: Text -> Text
categoryClass "regulation" = "money"
categoryClass "pricing"    = "money"
categoryClass _            = "plain"

--------------------------------------------------------------------------------
-- Text to HTML
--------------------------------------------------------------------------------

-- | Escape before anything else touches the text. Every renderer below assumes
-- its input has already been through here, which is why 'blocks' escapes once
-- at the top rather than each fragment escaping defensively.
esc :: Text -> Text
esc =
  T.replace "\"" "&quot;"
  . T.replace ">" "&gt;"
  . T.replace "<" "&lt;"
  . T.replace "&" "&amp;"

-- | Markdown as far as the composer actually writes it, and no further.
--
-- The prompts ask for paragraphs with no headings, so what arrives is blank-
-- line-separated prose with the occasional bullet list, bold run or link. A
-- full Markdown parser would be a dependency and a much larger surface for the
-- sake of syntax the prompts forbid; anything unrecognised falls through as the
-- literal characters, which is the right failure — a stray @#@ reads as a @#@
-- rather than silently restructuring the page.
blocks :: Text -> Text
blocks = T.concat . map block . splitBlocks . esc
  where
    block b
      | all bullet (T.lines b) =
          "<ul>" <> T.concat [ "<li>" <> spans (T.drop 2 (T.strip l)) <> "</li>"
                             | l <- T.lines b, not (T.null (T.strip l)) ]
                 <> "</ul>"
      | otherwise = "<p>" <> spans (T.unwords (map T.strip (T.lines b))) <> "</p>"
    bullet l = let t = T.strip l in T.null t || "- " `T.isPrefixOf` t

splitBlocks :: Text -> [Text]
splitBlocks =
  filter (not . T.null) . map T.strip . T.splitOn "\n\n" . T.replace "\r\n" "\n"

-- | Inline runs, strongest delimiter first so that @**@ is consumed before the
-- single-asterisk pass can see half of it.
spans :: Text -> Text
spans = links . between "*" "<em>" "</em>" . between "**" "<strong>" "</strong>"
      . between "`" "<code>" "</code>"

-- | Wrap the text between matched delimiters.
--
-- A delimiter that does not open a run is left exactly as it was found, and
-- "opens a run" is the rule Markdown itself uses: the run must be non-empty and
-- must not begin or end with a space. Without that, prose about @2 * 3 * 4@
-- italicises the middle of the sentence — the arithmetic is the common case in
-- pricing deltas, which is exactly where this stage is most used.
between :: Text -> Text -> Text -> Text -> Text
between delim open close = go
  where
    n = T.length delim

    go t = case T.breakOn delim t of
      (before, rest)
        | T.null rest -> before
        | otherwise ->
            let body = T.drop n rest
            in case run body of
                 Just (inner, after) -> before <> open <> inner <> close <> go after
                 -- Not a run: emit the delimiter literally and carry on from
                 -- just after it, so a later pair in the same line still pairs.
                 Nothing -> before <> delim <> go body

    run body = case T.breakOn delim body of
      (_, rest) | T.null rest -> Nothing
      (inner, rest)
        | T.null inner      -> Nothing
        | spaceAtEdge inner -> Nothing
        | otherwise         -> Just (inner, T.drop n rest)

    spaceAtEdge t =
      maybe False (isSpace . fst) (T.uncons t)
        || maybe False (isSpace . snd) (T.unsnoc t)

-- | @[text](url)@. A malformed link is left as its literal characters.
links :: Text -> Text
links t = case T.breakOn "[" t of
  (before, rest)
    | T.null rest -> before
    | otherwise ->
        let body = T.drop 1 rest
        in case T.breakOn "](" body of
             (_, r) | T.null r -> before <> "[" <> links body
             (label, r) ->
               case closeParen (T.drop 2 r) of
                 Nothing        -> before <> "[" <> links body
                 Just (url, r2) -> before <> link url label <> links r2

-- | Split a URL at the @)@ that closes the link, counting nested pairs.
--
-- Breaking at the first @)@ instead would truncate every URL that contains
-- one — Wikipedia's disambiguated titles are the case that shows up in
-- practice — leaving a broken href and a stray bracket in the prose.
closeParen :: Text -> Maybe (Text, Text)
closeParen = go (0 :: Int) []
  where
    go depth acc t = case T.uncons t of
      Nothing -> Nothing
      Just (')', rest)
        | depth == 0 -> Just (T.pack (reverse acc), rest)
        | otherwise  -> go (depth - 1) (')' : acc) rest
      Just ('(', rest) -> go (depth + 1) ('(' : acc) rest
      Just (c, rest)   -> go depth (c : acc) rest

-- | An anchor, or the label alone.
--
-- Only @http(s)@ and site-relative targets become links. The URLs on this page
-- come from feeds and from model output, and a @javascript:@ href in a page
-- served to other people is a real hole rather than a theoretical one; a
-- rejected target degrades to plain text, which is visible and harmless.
link :: Text -> Text -> Text
link url label
  | safe      = "<a href=\"" <> esc url <> "\" rel=\"noopener\">" <> esc label <> "</a>"
  | otherwise = esc label
  where
    safe = any (`T.isPrefixOf` T.toLower (T.strip url))
                ["http://", "https://", "/"]

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

-- | @2026-08-31@ becomes @Monday, 31 August 2026@. A date that will not parse
-- is shown as it was stored rather than replaced with a guess.
longDate :: Text -> Text
longDate raw =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack raw) :: Maybe Day of
    Just day -> T.pack (formatTime defaultTimeLocale "%A, %-d %B %Y" day)
    Nothing  -> raw

plural :: Int -> Text -> Text -> Text
plural n one many = tshow n <> " " <> (if n == 1 then one else many)

distinct :: [Text] -> [Text]
distinct = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

tshow :: Int -> Text
tshow = T.pack . show
