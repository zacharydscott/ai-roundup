# ai-roundup

Polls RSS/Atom feeds, remembers what it has already seen, and asks a model
through the Kudzu router for a roundup of what is new.

Built on [Conspire](../conspire), consumed from the working tree next door
rather than from Hackage — the same arrangement `kudzu-agent` uses.

## Running

```bash
ai-roundup run                      # collect → triage → group → resolve → compose → apply → publish
ai-roundup run --draft              # stop after the change set is written
ai-roundup apply 2026-08-30         # apply a change set written earlier
ai-roundup site build               # rebuild every page from the corpus
```

Sources come from `sources.yaml` (`--sources PATH` to point elsewhere).
`sources.smoke.yaml` is a one-feed cut of it for exercising the whole pipeline
in a few minutes; keep the real list in `sources.yaml` rather than trimming it
for a test run:

```bash
ai-roundup --store smoke.sqlite run --draft --sources sources.smoke.yaml
```

Every run writes its change set to `data/drafts/<date>/changes.json` **before**
anything touches the database, `--draft` or not. That file is the run's output
in reviewable form — every story it wants to create, every one it wants to
update, and the digest — and `apply <date>` turns it into rows. So a run that
dies after composition has not lost the day's work, and hand-editing the JSON
before applying is a supported way to correct one.

Every step is idempotent: an article the corpus already holds is dropped before
any model sees it, and applying the same change set twice writes the same rows.
That is what makes it safe to put on a timer.

## Publishing

`run` and `apply` write the day's page to `pages/roundups/<date>.html` after the
corpus has it, and refresh `pages/index.html`. `site build` rebuilds every page,
which is what you want after changing the template or hand-editing a story.

Pages are read back out of the database rather than rendered from the change
set, so what is published is what the corpus holds — a story whose slug already
existed keeps its stored prose, and a page built from the change set would show
prose that was discarded.

`pages/` is committed, and Cloudflare Pages serves it directly with no build
step (`wrangler.toml`): a push is a deploy. Cloudflare never needs a Haskell
toolchain, and a deploy cannot break in a way a local `site build` would not
have caught.

Under **Settings → Build**, all three commands stay empty — framework preset
**None**, build command **empty**, deploy command **empty** — with build output
directory `pages` and root directory `/`. The deploy command is the one that
matters and the one Cloudflare's onboarding is apt to fill in for you: a Pages
project with `npx wrangler deploy` set there runs the *Workers* deploy path
against `wrangler.toml`, reads it under Workers rules, and fails before
uploading anything. Pages uploads `pages_build_output_dir` natively and needs no
command to do it.

The digest a page renders is the ranked one: `digest_entry.position` is the
output of a model that compared every story of the day against every other, for
a reader who buys and governs AI rather than builds it. Rank 1 leads, 2–5 sit
above a marked break, and the rest follow.

`KUDZU_API_KEY` is required — resolution and composition are both model work and
there is no reduced run without them. Which model does that work is
`--source NAME`, looked up by name from the router at run time. `TAVILY_API_KEY`
is optional: without it the configured searches each report a failure and the
feeds still run.

## Layout

In pipeline order:

| Module | Does |
|---|---|
| `AI.Roundup.Config` | `sources.yaml`, parsed into typed config. |
| `AI.Roundup.Ingest` | What the sources said, mechanically deduplicated. `Ingest.Feed` fetches and parses RSS/Atom; `Ingest.Tavily` runs the standing searches over MCP. |
| `AI.Roundup.Triage` | Is this news, and if so what does it say? One short call per candidate, fanned out. |
| `AI.Roundup.Group` | Which of these articles are about the same event? |
| `AI.Roundup.Resolve` | Is this group new to the corpus, or more coverage of something already in it? |
| `AI.Roundup.Compose` | Prose: the write-ups and the digest intro. |
| `AI.Roundup.ChangeSet` | The record that flows from composition into application, and the JSON on disk. |
| `AI.Roundup.Apply` | The only code in the project that writes to the database. |
| `AI.Roundup.Store` | SQLite: schema, and the reads everything else is built on. |
| `AI.Roundup.Site` | The corpus as HTML: one page per digest, plus the archive index. |
| `Main` | CLI, and the seams between stages. |

One executable, no library: there is no second consumer yet. Modules live under
`src/` rather than beside `Main.hs`, so growing a library later is a stanza plus
moving one file.

## Dependency notes

**`feed` for parsing.** It covers RSS 0.9x/1.0/2.0 and Atom behind one parser
and one query API, which is the reason to take it — the alternative is branching
on document type at every call site. Verified against both a real RSS 2.0 feed
and a real Atom feed: titles, links, guids and dates all come through, and dates
parse to `UTCTime` from both RFC-822 and RFC-3339.

Three warts worth knowing. `getItemPublishDate` returns `Maybe (Maybe UTCTime)` —
outer is "no date field", inner is "date field that would not parse"; `Ingest/Feed.hs`
flattens it, since nothing downstream cares which. It pulls deprecated
`old-time` and `time-locale-compat` into the closure. And it does not fetch
anything: `http-client`/`http-client-tls` do that, which Conspire already
depends on, so there is no second TLS stack.

**`sqlite-simple` for storage.** `direct-sqlite` underneath compiles its own
copy of the SQLite C source, so the build needs no system library — `pkgs.sqlite`
is in the dev shell only for the `sqlite3` CLI.

## Versions

`flake.lock` is copied verbatim from `kudzu-agent`, which pins the same nixpkgs
revision Conspire does (`2c423e03bb…`, `nixos-unstable`). GHC 9.10.3. The
dependency closure is shared through `../conspire`, so these must not drift
apart independently.
