# ai-roundup — research → curation → daily roundup

Phase 1 plan, the phase 2 sketch it must not foreclose, and how the work splits.

## Context

The current program polls feeds, stores items in a flat table, and asks a model
for a one-shot roundup. That was a skeleton to prove the dependency stack
(`feed`, `sqlite-simple`, conspire through `../conspire`).

The product is different: an **evolving corpus of stories** — a model release, an
EU regulation — each accumulating coverage from many outlets over time, plus a
**daily digest** recording what broke and what got added that day. The reader
path is digest → story write-up → related stories.

Two properties drive most of the design:

- **Story dates are publication dates, never discovery dates.** A model released
  Monday, covered Wednesday, ingested Friday has `start_date` Monday. Only the
  digest is dated by discovery.
- **Duplicate coverage is grouped, never discarded.** Two feeds carrying the same
  story produce one story with two articles.

The existing schema is replaced outright — no migration path, no data worth
keeping.

## Design decisions that differ from the original brief

1. **`links` becomes an `article` table, not an array column.** Each link carries
   its own published date, title and outlet, and "group the duplicates" is
   literally many-articles-to-one-story. `last_updated` then falls out as
   `max(article.published_at)` instead of being a column that can drift.

2. **`first_seen` goes from the story, but `article.fetched_at` stays.** The
   story's dates become purely semantic as intended; the plumbing keeps its own
   clock, because "what changed since the last run" and "why didn't this appear"
   are unanswerable without it.

3. **`start_date` is anchored, with an LLM override.** Pure LLM date extraction is
   the least reliable step in the pipeline. Default to `min(article.published_at)`;
   the LLM may override when a source *states* the event date ("released on
   Tuesday"), recorded in `start_date_basis` (`earliest_coverage` |
   `stated_in_source`).

4. **`last_updated` is derived in the applier, never LLM-set.**

5. **The two streams are one resolver plus two queries.** Each candidate resolves
   to `New | Update story_id | Duplicate article_id | Ignore`. "Breaking" and
   "additional coverage" are a filter over that outcome. Two parallel pipelines
   would drift.

6. **Mechanical dedup runs before the LLM sees anything.** Canonical URL (strip
   `utm_*`, fragments, trailing slash), then guid, then exact title, then
   near-title. Most cross-feed duplicates die here for zero tokens.

7. **The resolver gets narrow typed tools, not raw SQL.** `search_stories(query,
   company?, category?, since?)` and `get_story(id)` returning compact JSON.
   Arbitrary SQL from a model against your own database is both a correctness risk
   and a context-blowout risk.

8. **Candidates are processed oldest-published first, and the resolver sees in-run
   creations.** Otherwise two articles about the same new story in one batch create
   two stories — the most likely corpus-corrupting bug here.

## Schema

Fresh `schema.sql`, applied with `IF NOT EXISTS` at open.

```
category(id, slug, label)                    -- seeded: model, regulation,
                                             --   business, pricing, general_tech
company(id, slug, name)
company_alias(company_id, alias)             -- LLM-proposed variants collapse here
model_family(id, company_id, slug, name)

story(id, slug, title,
      category_id            -> category,
      model_family_id        -> model_family NULL,
      summary,                              -- one-paragraph standing summary
      writeup_md,                           -- full evolving prose
      start_date, start_date_basis,
      last_updated,                         -- derived: max(article.published_at)
      vector_point_id NULL)                 -- reserved, unused in phase 1

story_company(story_id, company_id, is_primary)
story_relation(story_id, related_story_id, relation, note)

article(id, story_id, url_canonical UNIQUE, url_original, title,
        outlet, source_id, guid,
        published_at,                       -- from the feed/page
        fetched_at,                         -- our clock
        excerpt)

digest(id, digest_date UNIQUE, intro_md)    -- digest_date IS discovery date
digest_entry(digest_id, story_id, kind, delta_md, position)
                                            -- kind: 'breaking' | 'update'
                                            -- position: rank, 1 = most important
digest_article(digest_id, article_id)       -- the coverage the day gathered
                                            -- UNIQUE (digest_id, article_id)
```

`digest_entry` says which *stories* a day touched; `digest_article` says which
*articles* arrived to touch them. The second is not derivable from the first,
and not derivable from `article.fetched_at` either: fetched_at is when we first
saw a URL, so an article fetched on one day can reach a digest on another, an
article already in the corpus can be named again by later coverage, and a
rebuilt corpus stamps every row with the day it was rebuilt. Written by
`Apply.hs` from the article ids the story half of a change set actually stored,
which is what stops a digest pointing at coverage that was never applied.

`story_company` is a join table with `is_primary` because partnership
announcements and multi-vendor regulation are genuinely multi-company, and
aggregation by company is a stated goal. Single-company display queries read the
primary row.

Story prose lives in two places on purpose: `story.writeup_md` is the current full
text, `digest_entry.delta_md` is what changed *that day*. That gives the
"additions to existing entries" section real content without a revision table.

## Pipeline

> **Revised 2026-08-30 (phase 1.5).** The stage list below replaces the
> original three-stage middle. What changed and why is in "Phase 1.5" at the
> foot of this document; the sections between here and there describe the
> shipped design except where they discuss `RunLedger`, per-candidate
> resolution, or the `Ignore`/`Duplicate` verdicts, all of which are gone.

One run, seven stages, each a function over the previous:

```
1. collect    sources.yaml → [Candidate]
              RSS via existing fetchFeed; Tavily via MCP tools/call
2. dedup      mechanical: canonical URL, guid, title  → [Candidate] (grouped)
2b. known     drop candidates whose canonical URL is already stored (free)
3. triage     fan-out, one short call per candidate:
              Maybe CandidateSummary                  → [Triaged]
4. group      one call over every summary at once     → [CandidateGroup]
5. resolve    fan-out, per group: sub-agent +
              search_stories/get_story                → Resolution
6. compose    per touched story: rewrite writeup_md + delta_md
              per run: digest intro                   → DayChangeSet
7. apply      DayChangeSet → SQLite                   (the only writer)
```

**`DayChangeSet` is the spine.** A typed record serialised with the same
`JSONCodec` used for structured output, written to `data/drafts/<date>/changes.json`
before stage 5. Phase 1 applies immediately; the future editor flow is then "skip
stage 5" plus "apply this file", with no new write path.

Stages 3 and 4 use `structuredChat`
(`~/code/conspire/src/Conspire/TextGeneration.hs:123`) so output is typed rather
than parsed out of prose.

## Files

| Path | Contents |
|---|---|
| `sources.yaml` | Feed URLs and Tavily queries, with an id per source |
| `schema.sql` | Whole schema, embedded via `file-embed` |
| `src/AI/Roundup/Config.hs` | `sources.yaml` parsing (`yaml` package) |
| `src/AI/Roundup/Ingest.hs` | `Candidate` type, mechanical dedup, canonical URL |
| `src/AI/Roundup/Ingest/Feed.hs` | existing `Feed.hs`, moved |
| `src/AI/Roundup/Ingest/Tavily.hs` | MCP `tools/call` against the Tavily client |
| `src/AI/Roundup/ChangeSet.hs` | `DayChangeSet` + codecs + JSON read/write |
| `src/AI/Roundup/Triage.hs` | stage 3: relevance fan-out, `Maybe CandidateSummary` |
| `src/AI/Roundup/Group.hs` | stage 4: the partition, `CandidateGroup` |
| `src/AI/Roundup/Resolve.hs` | stage 5: per-group sub-agent, its two DB tools, `Resolution` |
| `src/AI/Roundup/Compose.hs` | write-up and digest prose generation |
| `src/AI/Roundup/Store.hs` | rewritten: schema, typed queries, aggregation reads |
| `src/AI/Roundup/Apply.hs` | `applyChangeSet` — the only thing that writes |
| `src/Main.hs` | subcommands |

Deleted: `src/AI/Roundup/Summary.hs` (superseded by `Compose.hs`).
Cabal additions: `yaml`, `aeson`, `containers`, `file-embed`.

### Reuse rather than reinvent

- **Codec pattern** — `objectCodec / req / opt / stringCodec / arrayCodec` from
  `Conspire.Codec.Json`. Worked example:
  `~/code/kudzu-agent/src/Kudzu/Agent/SubAgents/DeepResearch/Types.hs:57-80`.
- **Tool definition** — `Conspire.Tool.Tool`
  (`~/code/conspire/src/Conspire/Tool.hs:17`), same shape as `reportFindingTool`
  in the file above.
- **Sub-agent spawn** — `spawnSubAgentDI` + `buildToolMap` from
  `Conspire.Agent.Reader`; pattern at
  `~/code/kudzu-agent/src/Kudzu/Agent/SubAgents.hs:38-50`.
- **Tavily MCP client** — construction at `~/code/kudzu-agent/app/Main.hs:70-74`;
  invocation via `MCP.sendRequest client "tools/call"`
  (`~/code/conspire/src/Conspire/MCP.hs:157`).
- **Feed fetch/parse** — `fetchFeed` / `parseItems` already work against real RSS
  and Atom; they move, they don't change.

## CLI

```
ai-roundup run [--draft] [--date YYYY-MM-DD]   collect → … → apply
ai-roundup apply <date>                        apply an existing draft file
ai-roundup show story <slug>                   read back a write-up
ai-roundup show digest <date>
ai-roundup list --company X --category Y --since D
ai-roundup site build                          phase 2, generate site/
ai-roundup site deploy                         phase 2, BLOCKED on credentials
```

`--draft` stops after writing `data/drafts/<date>/changes.json`. Reruns of the same day
are idempotent: `digest.digest_date` is unique, article canonical URLs are unique,
and applying a changeset twice is a no-op.

## Work breakdown by subagent

The contracts have to be settled before anything fans out. Two agents writing
against a schema that is still moving produce code that compiles separately and
not together, so wave 0 is deliberately serial and deliberately small.

### Wave 0 — contracts (serial, one agent, no parallelism)

**A0 · Schema and ChangeSet types.** `schema.sql`, `Store.hs` types and the
open/init path, `ChangeSet.hs` types and codecs. No pipeline logic, no LLM calls.
Ends when `cabal build` passes and a hand-written `changes.json` round-trips
through the codec.

*Everything below imports these two modules. Nothing below starts until this
lands.*

### Wave 1 — three agents in parallel

**A1 · Ingest.** `Config.hs` (+ `sources.yaml`), move `Feed.hs` under `Ingest/`,
`Ingest/Tavily.hs`, `Ingest.hs` with canonical-URL and mechanical dedup.
Produces `[Candidate]`. Touches no LLM and no writes.
*Verifiable alone:* run against 4 real feeds and a Tavily query, dump candidates,
assert the cross-feed duplicate collapsed.

**A2 · Resolve.** `Resolve.hs`: the two read-only DB tools, the sub-agent, the
`Resolution` type. Consumes `[Candidate]`, produces `[Resolution]`.
*Verifiable alone:* seed a fixture DB, feed it hand-written candidates, assert
new/update/duplicate classification.

**A3 · Compose + Apply.** `Compose.hs` (write-up and digest prose) and `Apply.hs`
(`applyChangeSet`). Consumes `[Resolution]`, produces and then applies a
`DayChangeSet`.
*Verifiable alone:* hand-written `Resolution` list in, database state out.

The seam between A1→A2→A3 is three plain data types from wave 0, so the three can
be written against fixtures and never need to see each other's code.

### Wave 2 — integration (serial, one agent)

**A4 · Main wiring and end-to-end.** Subcommands, the run pipeline, drafts
directory layout, then the whole verification list below against live feeds. This
is where the real integration bugs surface — ordering, in-run creations, rerun
idempotency — and it wants one agent holding the whole picture, not three.

### Wave 3 — two agents in parallel, after wave 2 is green

**A5 · Static site generator.** `ai-roundup site build` → `site/`: one page per
story, one per digest, single-axis index pages, plus `index.json` for
client-side filtering. Reads the DB, writes files, deploys nothing.

**A6 · Docs.** README rewrite, sources.yaml documentation, the deferred-features
notes below.

### Wave 4 — BLOCKED

**A7 · Cloudflare Pages deploy.** Cannot be built or tested — no account
credentials yet. See below.

## The Cloudflare stopping point

Phase 2 stops one step short of deploying, on purpose. Everything up to the upload
is buildable and testable now; the upload itself is not, and a `deploy` path
written against an imagined account is a path nobody has run.

**What gets built:** `ai-roundup site build` producing a complete `site/` that can
be opened locally (`python3 -m http.server` in `site/`) and browsed end to end —
digest pages linking to story write-ups linking to related stories. `site/` is
committed to the repo.

**What gets stubbed:** `ai-roundup site deploy` exists, reads a `wrangler.toml`
with placeholder account id and project name, and refuses with a clear message
until those are filled in. No credentials are read, guessed, or stored.

**What you and the bot do manually, later:** create the Pages project, `wrangler
login`, fill in account id and project name, run the first deploy, confirm the
served site matches the local one. Only then is the deploy path worth automating.

One correction to the original brief: **no push manifest is needed.** `wrangler
pages deploy <dir>` hashes files and uploads only what changed. If a manifest is
wanted for another reason, derive it from `git status --porcelain` rather than
tracking it in the database, where it would drift from what is actually on disk.

## Deferred — noted, not built

- **Arbitrary aggregation combinations.** Phase 1 ships single-axis queries. The
  combinatorial case does **not** need a server: emit one `index.json` (id, date,
  companies, category, model family, title) and filter client-side. Cheaper and
  faster than a backend at this corpus size.
- **Vector lookup.** Reuse conspire's `VectorStore`
  (`~/code/conspire/src/Conspire/VectorStore.hs:13`, Qdrant and in-memory
  backends) over `story.writeup_md`, keyed by the reserved
  `story.vector_point_id`. Not sqlite-vec — the abstraction already exists next
  door.
- **Editor loop.** `--editor <date>` feeds notes plus the existing draft JSON back
  to the model for revision; `--publish <date>` applies it. Drafts live in
  `data/drafts/<date>/`, in the repo, never web-facing. Nearly free once the ChangeSet
  spine exists.
- **Schema migrations.** None in phase 1, by decision. The moment there is a
  corpus worth keeping, this needs a version table.

## Verification

1. `cabal build` in the dev shell.
2. `ai-roundup run --draft` against `sources.yaml` with 3–4 real AI feeds. Inspect
   `data/drafts/<date>/changes.json` by hand — this is the artefact the whole design
   rests on, and it should be readable without the schema in front of you.
3. `ai-roundup apply <date>`, then check with `sqlite3`:
   - `story.start_date` equals the earliest article's `published_at`, and is
     **not** today, for a story whose coverage predates the run;
   - `story.last_updated` equals the latest article's `published_at`;
   - `digest.digest_date` **is** today.
4. Seed the duplicate case deliberately: two feeds carrying one story (HN plus the
   vendor blog). Assert one `story` row, two `article` rows.
5. Rerun the same day unchanged → zero new stories, zero new articles, one digest
   row.
6. Run again a day later after new coverage lands → the story appears under
   `kind='update'` with a non-empty `delta_md`, and `last_updated` moves while
   `start_date` does not.
7. `ai-roundup list --company openai` and `--category regulation` return the
   expected sets.
8. `ai-roundup site build`, then serve `site/` locally and click through digest →
   story → related story. This is the last verifiable step before credentials.

## Phase 1.5 — triage, grouping, and the death of the ledger

Implemented 2026-08-30, after the first end-to-end run on live feeds.

### What went wrong with the original middle

Resolution ran one candidate at a time, sequentially, each a full sub-agent
conversation with the corpus tools attached. Three consequences, all measured
on a 122-candidate run:

- **Irrelevant candidates cost full price.** "Not worth keeping" was a resolver
  verdict, so a listicle bought a tool-equipped conversation before being
  discarded. Relevance is the cheapest judgement in the pipeline and it was
  being made at the most expensive stage.
- **It was slow enough to look broken.** Forty minutes with no output, because
  the stage emitted nothing until every candidate had finished.
- **It could not be widened.** Every feed added multiplied cost linearly, which
  ruled out exactly the feeds worth having: the ones that are mostly noise and
  occasionally the only place a story broke.

### The three changes

**Triage (stage 3) returns `Maybe CandidateSummary`.** One short, tool-free,
parallel call per candidate. `Nothing` means the candidate never reaches
another stage. A `Maybe` rather than a verdict record with a boolean, because a
record has to answer "what is the summary of a spiked candidate?" and every
answer is a `Just ""` — the value that looks present and reads as missing,
which `Ingest.nonEmpty` already exists to prevent. The wire shape keeps an
explicit `relevant` boolean, because an omitted field is a weaker signal than a
stated one for a small model; it collapses to `Maybe` at the codec boundary and
never escapes the module.

No reason string. It would be "not about AI" almost every time, and the
question an operator actually has is not "why was this one dropped" but "is
this feed pulling its weight". That is a per-source keep rate, printed from
data we already hold locally at zero token cost.

**Grouping (stage 4) sees every summary at once, and this deletes `RunLedger`.**
The old design's most intricate machinery — a ledger threaded through the fold,
a pending-stories list injected into every search result, a rule refusing a
`new` verdict until a search had run, and a repeat of the pending list in each
brief — were four mechanisms compensating for one fact: a candidate could not
see its neighbours. A grouper that reads the whole batch makes two articles
about one new story becoming two stories *unrepresentable*. All four went, and
with them the `PendingStory` half of `StoryRef`.

Nothing may be dropped in grouping. An index the model forgets, repeats, or
puts out of range still lands in exactly one group — its own, if nothing else
claims it. A malformed partition costs extra resolver calls and not one
article.

**Resolution (stage 5) runs per group, in parallel, with two outcomes.** Once
the partition is fixed, groups are independent. `Ignore` moved to triage;
`Duplicate` became a canonical-URL lookup costing nothing (stage 2b). What is
left is the one question needing both the corpus and a judgement: has this
event been written about before? The single shared resource is the slug
namespace, de-conflicted after the fan-out rather than serialised through it.

### Consequences

- `newManager`'s second argument is a global in-flight cap enforced by a queue.
  The fan-outs need no pool of their own; that number is the throttle.
- Reasoning level is set per source in code via `modifyStandardConfig`, not on
  the router. Router routes are shared with other programs, and a budget chosen
  for a few hundred triage calls has no business changing what they get.
- Sources widened from 4 feeds to 15. Anthropic, Meta, Microsoft, Mistral and
  xAI have no working feed as of 2026-08-30 — all 404 or 410 — so vendor
  coverage of those labs comes through the press, the individuals and the
  searches.

### Still open

- **arXiv is excluded.** 312 items in a day, all of them today's, so
  `max_age_days` removes none. It needs a mechanical keyword filter in front of
  it before triage is asked to pay for it.
- **The grouping call is single.** ~10k tokens for a few hundred summaries,
  fine at either model's context. A batch large enough to need splitting would
  also need a merge pass, and splitting it naively would reintroduce exactly
  the blindness this stage removed.
- **Composition still runs at low reasoning**, along with everything else. It
  is ~20 calls a run rather than several hundred, so it is the cheapest place
  to buy quality back if write-ups come out thin.
