{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point, CLI, and the wiring that makes five stages one run.
--
-- Each stage lives in its own module and knows nothing of its neighbours; what
-- is left over is here. That leftover is three seams, and every one of them is a
-- place a plausible-looking shortcut corrupts the corpus:
--
-- __Ingest to Resolve__ is a rename ('toResolveInput'). "AI.Roundup.Ingest"'s
-- 'Candidate' carries fetch-time detail the resolver has no use for; what
-- crosses is the article and the copies of it other feeds carried.
--
-- __Resolve to Compose__ is a fold ('foldResolved'), and it is the seam that
-- matters. Stage 3 decides per /candidate/; stage 4 writes per /story/, and one
-- story can be the outcome of several candidates. Folding them is the only way
-- two articles about one brand-new event become one story instead of two — and
-- the case it exists for is @Update (PendingStory slug)@, a story that has no
-- database id because it does not exist yet. Nothing here is allowed to reduce
-- that to a @Maybe Int64@: "no id" and "id unknown until apply time" are
-- different facts, and conflating them silently drops the creation.
--
-- __Compose to Apply__ is a file. The change set is written to
-- @data\/drafts\/\<date\>\/changes.json@ /before/ stage 5 runs, always, including
-- without @--draft@. Phase 1 then applies it immediately; the deferred editor
-- loop is "skip the apply" plus "apply this file", with no second write path to
-- keep in step. It is also the run's insurance: if the router dies between
-- composition and application, the day's work is on disk and @apply \<date\>@
-- finishes it offline.
--
-- __On @--date@.__ It sets the digest's date and nothing else. Story dates are
-- publication dates, which come off the articles, and @article.fetched_at@ is
-- our own clock, which stays real so that "why did this not appear" remains
-- answerable. So @--date@ reaches exactly one field, 'cinDate', and the
-- structure here is what keeps that true: no stage below is handed the date at
-- all.

module Main (main) where

import AI.Roundup.Apply (ApplyReport (..), applyChangeSet)
import AI.Roundup.Site (defaultPagesDir, writeRoundupPage, writeRoundupPages)
import AI.Roundup.ChangeSet
       ( ArticleSpec (..), DayChangeSet (..), StoryMeta (..)
       , readChangeSet, writeChangeSet )
import AI.Roundup.Compose
       ( ComposeInput (..), ComposeTarget (..), NewStoryInput (..)
       , UpdateStoryInput (..), composeChangeSet, lookupComposeSource )
import AI.Roundup.Config
       ( SourcesConfig (..), defaultSourcesPath, readSourcesConfig, sourceIds )
import AI.Roundup.Group
       ( CandidateGroup (..), groupPrimaryArticles, groupTriaged )
import AI.Roundup.Ingest
       ( Candidate (..), IngestFailure (..), candidateGroups, candidateSourceIds
       , collectCandidates, dedupCandidates )
import AI.Roundup.Ingest.Tavily (tavilyClientFromEnv)
import AI.Roundup.Resolve
       ( Resolution (..), ResolvedGroup (..), alreadyStored, defaultRouterUrl
       , resolveGroups, resolverProvider, resolverSource, routerKeyEnv )
import AI.Roundup.Triage
       ( CandidateSummary (..), TriageOutcome (..), Triaged (..), triageCandidates )
import AI.Roundup.Store
       ( ArticleRow (..), CategoryRow (..), DigestEntryRow (..), DigestRow (..)
       , DigestArticleRow (..), ListFilter (..), Store, StoryCompanyRow (..)
       , StoryDetailRow (..), StoryListItem (..), StoryRelationRow (..)
       , categories, closeStore, digestArticles, digestEntries, digestForDate
       , listStories, openStore, storyDetail, storyDetailForSlug )

import Conspire (Manager, newManager, runConspiracyWith)
import Conspire.MCP (Client)
import Conspire.Monad (Conspiracy, ConspiracyError (..))
import Conspire.Provider.Kudzu (KudzuProvider)

import Control.Exception (bracket)
import Control.Monad (filterM, forM, unless, when)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Natural (Natural)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO
       ( BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout )

--------------------------------------------------------------------------------
-- Subcommand arguments
--------------------------------------------------------------------------------

data RunArgs = RunArgs
  { runDraft   :: Bool
  , runDate    :: Maybe String
  , runSources :: FilePath
  , runSource  :: Text
  } deriving (Eq, Show)

data ApplyArgs = ApplyArgs
  { applyDate :: String
  } deriving (Eq, Show)

newtype ShowArgs = ShowArgs
  { showWhat :: ShowTarget
  } deriving (Eq, Show)

data ShowTarget
  = ShowStory String      -- slug
  | ShowDigest String     -- date
  deriving (Eq, Show)

data ListArgs = ListArgs
  { listCompany  :: Maybe Text
  , listCategory :: Maybe Text
  , listSince    :: Maybe Text
  , listLimit    :: Int
  } deriving (Eq, Show)

newtype SiteArgs = SiteArgs
  { siteCmd :: SiteCommand
  } deriving (Eq, Show)

data SiteCommand
  = SiteBuild
  | SiteDeploy
  deriving (Eq, Show)

data Command
  = CmdRun RunArgs
  | CmdApply ApplyArgs
  | CmdShow ShowArgs
  | CmdList ListArgs
  | CmdSite SiteArgs
  deriving (Eq, Show)

-- | The database is a global rather than a per-subcommand option because every
-- subcommand addresses the same corpus and none of them means anything without
-- it.
data Options = Options
  { optStore   :: FilePath
  , optCommand :: Command
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

runArgsP :: Parser RunArgs
runArgsP = RunArgs
  <$> switch (long "draft" <> help "Stop after writing data/drafts/<date>/changes.json")
  <*> optional (strOption (long "date" <> metavar "YYYY-MM-DD"
        <> help "Date to file this digest under (default: today). Story dates are \
                \publication dates and are not affected by this."))
  <*> strOption (long "sources" <> metavar "PATH" <> value defaultSourcesPath
        <> showDefault <> help "Feed and search configuration")
  <*> strOption (long "source" <> metavar "NAME" <> value "local" <> showDefault
        <> help "Router source that resolves and writes")

applyArgsP :: Parser ApplyArgs
applyArgsP = ApplyArgs
  <$> strArgument (metavar "DATE" <> help "Apply data/drafts/<DATE>/changes.json")

-- | @show story <slug>@ and @show digest <date>@: the target is a positional
-- word, not a flag, because "story" and "digest" name two different things to
-- read rather than two ways of reading one thing.
showArgsP :: Parser ShowArgs
showArgsP = ShowArgs <$> hsubparser
  ( command "story"
      (info (ShowStory <$> strArgument (metavar "SLUG"))
            (progDesc "Read back a story write-up"))
 <> command "digest"
      (info (ShowDigest <$> strArgument (metavar "DATE"))
            (progDesc "Read back one day's digest"))
  )

listArgsP :: Parser ListArgs
listArgsP = ListArgs
  <$> optional (strOption (long "company" <> metavar "NAME"
        <> help "Company name, slug or alias"))
  <*> optional (strOption (long "category" <> metavar "SLUG" <> help "Category slug"))
  <*> optional (strOption (long "since" <> metavar "DATE"
        <> help "Only stories updated on/after DATE"))
  <*> option auto (long "limit" <> metavar "N" <> value 100 <> showDefault
        <> help "Maximum stories to list")

siteArgsP :: Parser SiteArgs
siteArgsP = SiteArgs <$> hsubparser
  ( command "build"  (info (pure SiteBuild)  (progDesc "Generate site/"))
 <> command "deploy" (info (pure SiteDeploy) (progDesc "Deploy site/ (blocked on credentials)"))
  )

commandP :: Parser Command
commandP = hsubparser
  ( command "run"    (info (CmdRun <$> runArgsP) (progDesc "collect → resolve → compose → apply"))
 <> command "apply"  (info (CmdApply <$> applyArgsP) (progDesc "Apply an existing draft"))
 <> command "show"   (info (CmdShow <$> showArgsP) (progDesc "Read back a story or digest"))
 <> command "list"   (info (CmdList <$> listArgsP) (progDesc "List stories with filters"))
 <> command "site"   (info (CmdSite <$> siteArgsP) (progDesc "Static site generation"))
  )

optionsP :: Parser Options
optionsP = Options
  <$> strOption (long "store" <> metavar "PATH" <> value defaultStorePath
        <> showDefault <> help "SQLite corpus")
  <*> commandP

-- | Beside @sources.yaml@ and @data\/drafts\/@, because the corpus, the sources
-- that built it and the drafts that changed it are one working set.
defaultStorePath :: FilePath
defaultStorePath = "roundup.sqlite"

main :: IO ()
main = do
  -- A run is minutes of sequential model turns, and these lines are the only
  -- view an operator has of it. Both handles block-buffer as soon as they are
  -- redirected, which turns @run | tee@ and @run > log@ into silence until the
  -- process exits — exactly when the progress has stopped being useful.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  opts <- execParser $ info (optionsP <**> helper) $ mconcat
    [ fullDesc
    , progDesc "AI news roundup: research → curation → daily digest"
    , header "ai-roundup — a feed-watching agent built on Conspire"
    ]
  let db = optStore opts
  case optCommand opts of
    CmdRun a   -> cmdRun db a
    CmdApply a -> cmdApply db a
    CmdShow a  -> cmdShow db (showWhat a)
    CmdList a  -> cmdList db a
    CmdSite a  -> cmdSite db (siteCmd a)

--------------------------------------------------------------------------------
-- run
--------------------------------------------------------------------------------

-- | The whole pipeline.
--
-- The order of the four things that must happen before the database is touched
-- is deliberate: the date is settled first (a malformed @--date@ should cost
-- nothing), then the config (a broken @sources.yaml@ likewise), then the
-- collection, and only then is the store opened. Stages 1 and 2 need neither a
-- model nor the corpus, so a run that is going to fail for a bad feed list fails
-- before it has spent a token.
cmdRun :: FilePath -> RunArgs -> IO ()
cmdRun db args = do
  date <- resolveDate (runDate args)
  cfg  <- loadSources (runSources args)
  mgr  <- newManager runTimeoutMs inFlightCap

  -- Stages 1-2.
  say ("collecting from " <> tshow (length (sourceIds cfg)) <> " source(s): "
       <> commas (sourceIds cfg))
  (candidates, failures) <- collect mgr cfg
  for_ failures $ \f ->
    warn ("source " <> ifSourceId f <> " failed: " <> ifReason f)
  -- Every source failing is not a quiet news day, and a digest row saying it was
  -- one is worse than no digest row: it is indistinguishable from the real thing
  -- for every later read.
  when (length failures == length (sourceIds cfg)) $
    die "every source failed; refusing to record the day as empty"
  let deduped = dedupCandidates candidates
  reportIngest candidates deduped

  -- Stages 3-4. The store is opened before either, because @openStore@ owns the
  -- schema and 'applyChangeSet' refuses a database that has never seen it.
  targets <- bracket (openStore db) closeStore $ \store -> do
    -- The free filter, before any model sees anything. An article the corpus
    -- already has costs nothing to recognise, and this is what makes rerunning
    -- a day free rather than merely idempotent.
    fresh <- filterM (fmap not . alreadyStored store) deduped
    say (tshow (length deduped - length fresh) <> " already stored, "
         <> tshow (length fresh) <> " to triage")
    if null fresh
      then pure []
      else do
        provider <- requireProvider
        cats <- map categorySlug <$> categories store

        say ("triaging " <> tshow (length fresh) <> " candidate(s) via source "
             <> runSource args <> " — parallel, one short call each")
        triaged <- runOrDie mgr $ do
          src <- lookupComposeSource provider (runSource args)
          triageCandidates src cats reportTriaged fresh
        reportTriage fresh triaged

        say ("grouping " <> tshow (length triaged) <> " summarie(s) into events")
        groups <- runOrDie mgr $ do
          src <- lookupComposeSource provider (runSource args)
          groupTriaged src triaged
        reportGroups groups

        say ("resolving " <> tshow (length groups) <> " group(s) against the corpus")
        resolved <- runOrDie mgr $ do
          src <- resolverSource provider (runSource args)
          resolveGroups store src reportResolvedGroup groups
        reportResolutions resolved

        let touched = foldResolved resolved
        (targets, notes) <- composeTargets store touched
        for_ notes warn
        pure targets

  changes <-
    if null targets
      -- A day on which nothing resolved to a story still gets a change set and
      -- still gets a digest row: "we looked and there was nothing" is a fact
      -- about the corpus. There is nothing to write about, so no model is asked
      -- to write it.
      then do
        say "nothing to compose; recording an empty day"
        pure (emptyChangeSet date)
      else do
        provider <- requireProvider
        say ("composing " <> tshow (length targets) <> " storie(s) plus the intro")
        runOrDie mgr $ composeChangeSet provider (runSource args) reportComposed
          ComposeInput { cinDate = date, cinTargets = targets }

  -- Before stage 5, always. See the module header.
  draft <- writeDraft date changes
  say ("wrote " <> T.pack draft)

  if runDraft args
    then say "--draft: stopping before apply"
    else do
      applyChangeSet db changes >>= reportApply
      publish db date

-- | Feeds and searches, in one 'Conspiracy' run.
--
-- Two HTTP managers, which is not an oversight: @fetchFeed@ wants
-- @http-client@'s manager and conspire wraps its own in a record that does not
-- give the underlying one back. They are connection pools, not state, so two is
-- a cost of nothing.
collect :: Manager -> SourcesConfig -> IO ([Candidate], [IngestFailure])
collect mgr cfg = do
  httpMgr <- newTlsManager
  client  <- tavilyClient
  runOrDie mgr (collectCandidates httpMgr client cfg)

-- | The Tavily client, or 'Nothing'.
--
-- Not fatal either way: "AI.Roundup.Ingest" turns a missing client into one
-- reported failure per configured search, which is the honest answer — a
-- roundup built from feeds alone is a different roundup, and it should say so
-- rather than look like a thin day.
tavilyClient :: IO (Maybe Client)
tavilyClient = do
  result <- tavilyClientFromEnv
  case result of
    Nothing -> pure Nothing
    Just (Left err) -> warn ("tavily client: " <> err) >> pure Nothing
    Just (Right c)  -> pure (Just c)

-- | The router provider, or a stop. Unlike Tavily this is not degradable:
-- resolution and composition are both model work, and there is no reduced run
-- without them.
requireProvider :: IO KudzuProvider
requireProvider = do
  mProvider <- resolverProvider defaultRouterUrl
  case mProvider of
    Just p  -> pure p
    Nothing -> die (routerKeyEnv <> " is not set; resolution and composition need it")

-- | The per-request ceiling. Deliberately enormous.
--
-- Nothing here streams: conspire's structured and tool paths both send
-- @stream: false@ (@Conspire.Provider.Kudzu@ line 233), so the server holds the
-- entire completion until generation finishes and only then answers. That
-- makes this a ceiling on /whole-generation time/, not on time-to-first-byte —
-- there is no partial output to observe, and a request that has produced 90% of
-- a long write-up looks exactly like one that has produced nothing.
--
-- Ten minutes was the old value and it was not obviously wrong; what actually
-- broke a 224-candidate run was requests spending their ceiling queued rather
-- than generating, which 'Conspire.Manager.withSlot' now prevents. This is
-- raised anyway, because the cost of the two mistakes is not symmetric: too
-- long wastes a slot on a request that was never coming back, while too short
-- discards a completion that was nearly finished and bills for it regardless.
--
-- Switching any stage to the streaming variants would make a much shorter
-- ceiling correct, since a stalled stream is detectable in seconds. Until then
-- this is the number that has to cover the worst case.
runTimeoutMs :: Int
runTimeoutMs = 1800000

-- | How many requests may be in flight at once, across the whole run.
--
-- Enforced by a queue inside conspire's manager, which is what stops a fan-out
-- of a few hundred from opening a few hundred concurrent requests against one
-- local deployment and starving all of them.
--
-- Four is a serving-concurrency guess for a single 27B, not a measurement.
-- Raise it if the box has headroom — throughput here is the product of this
-- number and the model's speed, and the fan-out stages are the whole reason
-- the pipeline was reshaped. Note the interaction with 'runTimeoutMs': a hung
-- request holds its slot for the full ceiling, so at four, two stuck requests
-- halve the run's throughput for half an hour.
inFlightCap :: Natural
inFlightCap = 4

--------------------------------------------------------------------------------
-- Seam: Resolve → Compose
--------------------------------------------------------------------------------

-- | The stories one run touched, in the order it touched them.
--
-- Association lists rather than maps because order is the point — groups reach
-- this stage oldest-event first, and keeping that order means a story's
-- articles reach the writer in the order things happened — and because a day is
-- tens of entries, where a map would buy nothing.
data Touched = Touched
  { tchNew    :: [(StoryMeta, [ArticleSpec])]  -- ^ minted this run, creation order
  , tchUpdate :: [(Int64, [ArticleSpec])]      -- ^ already stored, first-touch order
  }

-- | Group resolutions by the story they touch.
--
-- Much smaller than it was. When resolution ran per candidate this had to
-- reunite the several candidates that turned out to be one story, including
-- the ones naming a story minted moments earlier in the same run and therefore
-- having no id yet. "AI.Roundup.Group" now settles that before the corpus is
-- consulted, so one group arrives as one story and the only thing left to
-- merge is two groups the resolver independently matched to the same stored
-- story — which is a real possibility and the reason 'bump' still exists.
foldResolved :: [ResolvedGroup] -> Touched
foldResolved = foldl' step (Touched [] [])
  where
    step t r = case rgResolution r of
      New meta -> t { tchNew = tchNew t ++ [(meta, arts)] }
      Update sid -> t { tchUpdate = bump sid arts (tchUpdate t) }
      where arts = groupPrimaryArticles (rgGroup r)

    bump sid a [] = [(sid, a)]
    bump sid a ((s, as) : rest)
      | s == sid  = (s, as ++ a) : rest
      | otherwise = (s, as) : bump sid a rest

-- | Turn touched stories into composition targets.
--
-- An update has to be given the story's /current/ prose, because composing an
-- update means rewriting the standing write-up with the new coverage folded in
-- rather than appending to it — so this is where 'storyDetail' is read, at the
-- last moment before the writer needs it.
composeTargets :: Store -> Touched -> IO ([ComposeTarget], [Text])
composeTargets store t = do
  updates <- forM (tchUpdate t) $ \(sid, arts) -> do
    detail <- storyDetail store sid
    pure $ case detail of
      Nothing -> Left ("update refers to story id " <> tshow sid
                       <> ", which is not in the corpus; its coverage is dropped")
      Just d  -> Right $ TargetUpdate UpdateStoryInput
        { usiStoryId     = sdrId d
        , usiSlug        = sdrSlug d
        , usiTitle       = sdrTitle d
        , usiCategory    = sdrCategory d
        , usiSummary     = sdrSummary d
        , usiWriteup     = sdrWriteup d
        , usiNewArticles = arts
        }
  pure ( [ TargetNew (NewStoryInput meta arts) | (meta, arts) <- tchNew t ]
           ++ [ target | Right target <- updates ]
       , [ n | Left n <- updates ]
       )

-- | A day with no stories. Still a digest row, still idempotent on rerun.
emptyChangeSet :: Text -> DayChangeSet
emptyChangeSet date = DayChangeSet
  { dcsDate          = date
  , dcsIntro         = Nothing
  , dcsNewStories    = []
  , dcsUpdates       = []
  , dcsDigestEntries = []
  }

--------------------------------------------------------------------------------
-- apply
--------------------------------------------------------------------------------

cmdApply :: FilePath -> ApplyArgs -> IO ()
cmdApply db args = do
  date <- resolveDate (Just (applyDate args))
  let path = draftPath date
  present <- doesFileExist path
  unless present $ die (T.pack path <> ": no draft there")
  parsed <- readChangeSet path
  changes <- either (die . ((T.pack path <> ": ") <>) . T.pack) pure parsed
  -- A draft whose own date disagrees with the directory it sits in will file the
  -- digest under the date inside the file. Worth saying out loud, not worth
  -- refusing: hand-editing a date is a legitimate thing to do.
  when (dcsDate changes /= date) $
    warn ("draft is dated " <> dcsDate changes <> " but sits under " <> date
          <> "; the digest will be filed under " <> dcsDate changes)
  -- The schema belongs to @openStore@; 'applyChangeSet' refuses a database
  -- without it rather than carrying a second copy of the DDL.
  bracket (openStore db) closeStore (const (pure ()))
  applyChangeSet db changes >>= reportApply
  publish db (dcsDate changes)

-- | Write the page for one day, after the corpus has it.
--
-- Reads the day back out of the database rather than rendering the change set,
-- so the page says what the corpus says. A story whose slug already existed
-- keeps its stored prose (see "AI.Roundup.Apply"), and a page built from the
-- change set would show the prose that was discarded.
--
-- Failing to publish does not fail the command: the corpus is the product and
-- it is already written by this point, while the pages can be rebuilt from it
-- at any time with @site build@.
publish :: FilePath -> Text -> IO ()
publish db date = do
  written <- bracket (openStore db) closeStore $ \store ->
    writeRoundupPage store defaultPagesDir date
  case written of
    Just path -> say ("wrote " <> T.pack path)
    Nothing   -> warn ("no digest for " <> date <> "; no page written")

--------------------------------------------------------------------------------
-- show
--------------------------------------------------------------------------------

cmdShow :: FilePath -> ShowTarget -> IO ()
cmdShow db target = bracket (openStore db) closeStore $ \store -> case target of
  ShowStory slug -> do
    detail <- storyDetailForSlug store (T.pack slug)
    maybe (die ("no story with slug " <> T.pack slug)) printStory detail
  ShowDigest raw -> do
    date <- resolveDate (Just raw)
    digest <- digestForDate store date
    case digest of
      Nothing -> die ("no digest for " <> date)
      Just d  -> do
        entries <- digestEntries store (digestId d)
        arts    <- digestArticles store (digestId d)
        printDigest store d entries arts

printStory :: StoryDetailRow -> IO ()
printStory d = do
  out (sdrTitle d)
  out (T.replicate (T.length (sdrTitle d)) "=")
  out ("slug:         " <> sdrSlug d)
  out ("category:     " <> sdrCategory d)
  for_ (sdrModelFamily d) $ \f -> out ("model family: " <> f)
  out ("companies:    " <> commas [ scrName c <> if scrPrimary c then " (primary)" else ""
                                  | c <- sdrCompanies d ])
  out ("start date:   " <> orNone (sdrStartDate d)
       <> maybe "" (\b -> " (" <> b <> ")") (sdrStartBasis d))
  out ("last updated: " <> orNone (sdrLastUpdated d))
  -- Labelled, because the standing summary and the write-up are two different
  -- things stored in two different columns and they read as one long article
  -- when printed back to back without saying so.
  for_ (sdrSummary d) $ \s -> out "" >> out "SUMMARY" >> out s
  for_ (sdrWriteup d) $ \w -> out "" >> out "WRITE-UP" >> out w
  out ""
  out ("COVERAGE (" <> tshow (length (sdrArticles d)) <> ")")
  for_ (sdrArticles d) $ \a -> out
    ("  " <> orNone (artPublishedAt a) <> "  "
     <> orNone (artOutlet a) <> " — " <> orNone (artTitle a)
     <> "\n    " <> artUrlCanonical a)
  unless (null (sdrRelations d)) $ do
    out ""
    out "RELATED"
    for_ (sdrRelations d) $ \r -> out
      ("  " <> srrRelation r <> ": " <> srrRelatedSlug r
       <> maybe "" (" — " <>) (srrNote r))

printDigest :: Store -> DigestRow -> [DigestEntryRow] -> [DigestArticleRow] -> IO ()
printDigest store d entries arts = do
  out ("DIGEST " <> digestDate d)
  out (T.replicate (7 + T.length (digestDate d)) "=")
  for_ (digestIntro d) $ \i -> out "" >> out i
  for_ entries $ \e -> do
    -- The digest stores slugs; a reader wants titles. One lookup per entry, and
    -- a digest is a handful of entries.
    detail <- storyDetailForSlug store (derStorySlug e)
    out ""
    out ("[" <> derKind e <> "] " <> maybe (derStorySlug e) sdrTitle detail)
    out ("  " <> derStorySlug e)
    for_ (derDelta e) $ \delta -> out ("  " <> delta)
  -- The day's coverage, not the stories' coverage: these are the articles this
  -- digest gathered, which is what @digest_article@ records and the one thing
  -- @show story@ cannot tell you.
  unless (null arts) $ do
    out ""
    out ("COVERAGE (" <> tshow (length arts) <> ")")
    for_ arts $ \a -> do
      let art = darArticle a
      out ("  " <> orNone (artPublishedAt art) <> "  "
           <> orNone (artOutlet art) <> " — " <> orNone (artTitle art)
           <> "\n    " <> artUrlCanonical art
           <> "\n    " <> darStorySlug a)

--------------------------------------------------------------------------------
-- list
--------------------------------------------------------------------------------

cmdList :: FilePath -> ListArgs -> IO ()
cmdList db args = bracket (openStore db) closeStore $ \store -> do
  items <- listStories store ListFilter
    { lfCompany  = listCompany args
    , lfCategory = listCategory args
    , lfSince    = listSince args
    , lfLimit    = listLimit args
    }
  if null items
    then out "no stories match"
    else do
      -- Two bare date columns are ambiguous, and which one is which is the
      -- distinction the whole corpus turns on.
      out (row "STARTED" "UPDATED" "CATEGORY" "COMPANIES" "STORY")
      for_ items $ \i -> out (row
        (dayOf (litStartDate i))
        (dayOf (litLastUpdated i))
        (litCategory i)
        (commas (litCompanies i))
        (litTitle i <> "  (" <> litSlug i <> ")"))
  where
    row a b c d e = T.intercalate "  "
      [ pad 10 a, pad 10 b, pad 14 c, pad 22 d, e ]
    dayOf = T.take 10 . orNone

--------------------------------------------------------------------------------
-- site (wave 3)
--------------------------------------------------------------------------------

-- | @site build@ rebuilds every page from the corpus. @run@ and @apply@ already
-- write the day they touched, so this is for a changed template, a hand-edited
-- story, or a first build against a corpus that predates the generator.
cmdSite :: FilePath -> SiteCommand -> IO ()
cmdSite db SiteBuild = do
  paths <- bracket (openStore db) closeStore $ \store ->
    writeRoundupPages store defaultPagesDir
  for_ paths $ \p -> say ("wrote " <> T.pack p)
  say (tshow (length paths) <> " page(s) under " <> T.pack defaultPagesDir)
-- Deploy is Cloudflare's, not ours: @wrangler.toml@ points Pages at @pages/@ and
-- a push deploys it. A deploy subcommand here would be a second way to do it
-- that could disagree with the first.
cmdSite _ SiteDeploy = die
  "deploys happen on push — Cloudflare Pages serves pages/ (see wrangler.toml). \
  \Run 'site build' and commit the result."

--------------------------------------------------------------------------------
-- Dates and paths
--------------------------------------------------------------------------------

-- | Today, or the given day.
--
-- Round-tripped through 'Day' rather than taken as text, so @--date 2026-8-3@
-- and @--date yesterday@ are both refused here rather than reaching the digest
-- table as a string that sorts wrongly for the rest of the corpus's life.
resolveDate :: Maybe String -> IO Text
resolveDate Nothing = T.pack . show . utctDay <$> getCurrentTime
resolveDate (Just raw) =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" raw :: Maybe Day of
    Just day -> pure (T.pack (show day))
    Nothing  -> die ("not a date in YYYY-MM-DD form: " <> T.pack raw)

-- | @data\/drafts\/\<date\>\/changes.json@ — in the repo, and never web-facing.
--
-- A directory per date rather than a file per date because the editor loop will
-- want notes and revisions beside the change set. Under @data\/@ rather than at
-- the root because drafts are the first of the run's outputs and will not be the
-- last: the generated site and any exports belong beside them, under one
-- directory that says at a glance which parts of the tree the program writes and
-- which parts a person does.
draftsDir :: FilePath
draftsDir = "data" </> "drafts"

draftPath :: Text -> FilePath
draftPath date = draftDir date </> "changes.json"

draftDir :: Text -> FilePath
draftDir date = draftsDir </> T.unpack date

writeDraft :: Text -> DayChangeSet -> IO FilePath
writeDraft date changes = do
  let path = draftPath date
  createDirectoryIfMissing True (draftDir date)
  writeChangeSet path changes
  pure path

loadSources :: FilePath -> IO SourcesConfig
loadSources path = readSourcesConfig path >>= either die pure

--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------

-- | What ingest found, and what dedup did to it. Both numbers matter: the first
-- says whether the sources answered, the second says whether dedup is working,
-- and a run where they are equal on a day with four overlapping feeds is a
-- symptom rather than a fact.
reportIngest :: [Candidate] -> [Candidate] -> IO ()
reportIngest raw deduped = do
  say (tshow (length raw) <> " candidate(s) collected, "
       <> tshow (length deduped) <> " after mechanical dedup ("
       <> tshow (length (candidateGroups deduped)) <> " group(s))")
  for_ deduped $ \c -> do
    let sources = candidateSourceIds c
    when (length sources > 1) $
      say ("  collapsed " <> tshow (length sources) <> " copies ["
           <> commas sources <> "] " <> asTitle (cndArticle c))

-- | One line per candidate as triage finishes it.
--
-- The counter is completions, not position: the fan-out finishes out of order
-- and a column that pretended otherwise would show a sequence that never
-- happened. Spikes are printed as loudly as keeps — a spike is invisible
-- everywhere else, and a feed quietly losing real coverage is the one failure
-- this stage can cause.
reportTriaged :: Int -> Int -> Candidate -> TriageOutcome -> IO ()
reportTriaged i total cand outcome = say
  ("  " <> counter i total <> " " <> pad 8 verdict
   <> "  [" <> asSourceId (cndArticle cand) <> "] "
   <> asTitle (cndArticle cand) <> detail)
  where
    (verdict, detail) = case outcome of
      Kept s   -> ("keep", "  — " <> csCategory s)
      Spiked   -> ("spike", "")
      Failed m -> ("FAILED", "  — " <> m)

-- | What triage did, per source.
--
-- A rate rather than a list of reasons, because the actionable question is
-- never "why was this one dropped" but "is this feed pulling its weight or is
-- it misconfigured". A source at @0/312@ is one or the other, and no per-item
-- reason string would tell you which.
reportTriage :: [Candidate] -> [Triaged] -> IO ()
reportTriage input kept = do
  say (tshow (length kept) <> " kept, " <> tshow (length input - length kept)
       <> " spiked or failed")
  for_ sources $ \s ->
    say ("  " <> pad 24 s <> "  " <> tshow (keptFrom s) <> "/" <> tshow (sawFrom s))
  where
    sourceOf = asSourceId . cndArticle
    sources = nubOrd (map sourceOf input)
    sawFrom s = length [ () | c <- input, sourceOf c == s ]
    keptFrom s = length [ () | t <- kept, sourceOf (tgCandidate t) == s ]
    nubOrd = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

-- | The partition, so a bad one is visible before it costs resolver calls.
reportGroups :: [CandidateGroup] -> IO ()
reportGroups groups = do
  say (tshow (length groups) <> " group(s)")
  for_ groups $ \g -> do
    let n = length (cgMembers g)
    say ("  " <> pad 6 (tshow n <> (if n == 1 then " art" else " arts"))
         <> "  " <> pad 14 (cgCategory g) <> "  " <> cgTitle g)

-- | One line per group as the corpus decision lands.
reportResolvedGroup :: Int -> Int -> ResolvedGroup -> IO ()
reportResolvedGroup i total r = say
  ("  " <> counter i total <> " " <> pad 24 (describeResolution (rgResolution r))
   <> "  " <> cgTitle (rgGroup r))

describeResolution :: Resolution -> Text
describeResolution res = case res of
  New meta -> "new " <> smSlug meta
  Update i -> "update #" <> tshow i

-- | @[  7/122]@, right-aligned so the lines stay in one column.
counter :: Int -> Int -> Text
counter i total =
  "[" <> T.justifyRight (T.length (tshow total)) ' ' (tshow i) <> "/" <> tshow total <> "]"

-- | One line per composed story. 'AI.Roundup.Compose.ComposeProgress' reports
-- the digest intro with position @0@, which is why the counter is dropped for
-- it rather than printed as @0/n@.
reportComposed :: Int -> Int -> Text -> Text -> IO ()
reportComposed 0 _ kind title = say ("  " <> pad 24 kind <> "  " <> title)
reportComposed i total kind title = say
  ("  [" <> T.justifyRight (T.length (tshow total)) ' ' (tshow i) <> "/" <> tshow total
   <> "] " <> pad 24 kind <> "  " <> title)

-- | The tally. The per-group lines have already streamed past by the time this
-- runs, so it counts rather than repeating them.
reportResolutions :: [ResolvedGroup] -> IO ()
reportResolutions resolved =
  say (tshow (length resolved) <> " resolved: "
       <> commas [ tshow n <> " " <> label
                 | (label, n) <- tallies, n > (0 :: Int) ])
  where
    tallies =
      [ ("new",    count (\r -> case r of New{} -> True; _ -> False))
      , ("update", count (\r -> case r of Update{} -> True; _ -> False))
      ]
    count p = length (filter (p . rgResolution) resolved)

-- | 'arpSkipped' is the only channel through which a change set that named
-- something absent can announce itself, so it is printed rather than counted.
reportApply :: ApplyReport -> IO ()
reportApply r = do
  say (commas
    [ tshow (arpStoriesCreated r) <> " story(ies) created"
    , tshow (arpStoriesUpdated r) <> " updated"
    , tshow (arpArticlesInserted r) <> " article(s) inserted"
    , (if arpDigestCreated r then "digest created" else "digest already present")
    , tshow (arpEntriesWritten r) <> " digest entry(ies) written"
    , tshow (arpArticlesLinked r) <> " article(s) linked to the day"
    ])
  for_ (arpSkipped r) $ \s -> warn ("skipped: " <> s)

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

-- | Run a 'Conspiracy' action, or stop with its error.
--
-- The observability log is discarded: conspire collects it for callers that
-- render a trace, and this program's trace is the lines it prints as it goes.
runOrDie :: Manager -> Conspiracy a -> IO a
runOrDie mgr act = do
  (result, _log) <- runConspiracyWith mgr act
  either (die . conspiracyError) pure result

-- | Progress: what a run is doing while it does it. Diagnostics, so stderr —
-- which also keeps it out of the way of the commands whose output is data.
say :: Text -> IO ()
say = TIO.hPutStrLn stderr

-- | Payload: a write-up, a digest, a list. The thing the caller asked for, on
-- stdout, so @show story \<slug\> > page.md@ gets the story and nothing else.
out :: Text -> IO ()
out = TIO.putStrLn

warn :: Text -> IO ()
warn msg = hPutStrLn stderr ("ai-roundup: " ++ T.unpack msg)

die :: Text -> IO a
die msg = warn msg >> exitFailure

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Left-justify to a column. Shared by the story list and the progress lines
-- so a run's output lines up down the page.
pad :: Int -> Text -> Text
pad n = T.justifyLeft n ' '

commas :: [Text] -> Text
commas = T.intercalate ", "

orNone :: Maybe Text -> Text
orNone = fromMaybe "(none)"
