# ai-roundup

Polls RSS/Atom feeds, remembers what it has already seen, and asks a model
through the Kudzu router for a roundup of what is new.

Built on [Conspire](../conspire), consumed from the working tree next door
rather than from Hackage — the same arrangement `kudzu-agent` uses.

## Running

```bash
ai-roundup --feed https://hnrss.org/frontpage \
           --feed https://blog.rust-lang.org/feed.xml \
           --store ~/.local/share/ai-roundup.sqlite
```

Every step is idempotent: polling twice stores nothing the second time, and
items are marked as summarised only once a roundup has actually come back. That
is what makes it safe to put on a timer.

`KUDZU_ROUTER_KEY` is required to summarise, but not to poll — a run without one
still fetches and stores, then reports how many items are waiting and exits
non-zero. Which model writes the roundup is `--source NAME`, looked up by name
from the router at run time.

## Layout

| Module | Does |
|---|---|
| `AI.Roundup.Feed` | Fetch and parse. Resolves `feed`'s pervasive optionality into a `FeedItem` that always has an identity and a title. |
| `AI.Roundup.Store` | SQLite. Dedup on `(source, guid)`, and track which items have made it into a roundup. |
| `AI.Roundup.Summary` | The one Conspire call. |
| `Main` | CLI and glue. |

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
outer is "no date field", inner is "date field that would not parse"; `Feed.hs`
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
