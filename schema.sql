-- ai-roundup schema, applied at open with IF NOT EXISTS.
--
-- The corpus is a set of evolving stories, each accumulating article coverage
-- from many outlets over time, plus daily digests recording what broke and what
-- was added that day. Story dates are publication dates, never discovery dates:
-- start_date anchors the event (earliest coverage unless the source states it),
-- and last_updated is derived as max(article.published_at) by the applier.
-- Discovery time is not dropped entirely; it lives on article.fetched_at so
-- "what changed since the last run" and "why didn't this appear" stay answerable.

CREATE TABLE IF NOT EXISTS category (
    id    INTEGER PRIMARY KEY,
    slug  TEXT NOT NULL UNIQUE,
    label TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS company (
    id   INTEGER PRIMARY KEY,
    slug TEXT NOT NULL UNIQUE,
    name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS company_alias (
    company_id INTEGER NOT NULL REFERENCES company(id),
    alias      TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS model_family (
    id           INTEGER PRIMARY KEY,
    company_id   INTEGER NOT NULL REFERENCES company(id),
    slug         TEXT NOT NULL UNIQUE,
    name         TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS story (
    id                INTEGER PRIMARY KEY,
    slug              TEXT NOT NULL UNIQUE,
    title             TEXT NOT NULL,
    category_id       INTEGER NOT NULL REFERENCES category(id),
    model_family_id   INTEGER REFERENCES model_family(id),
    summary           TEXT,
    writeup_md        TEXT,
    start_date        TEXT,
    start_date_basis  TEXT,
    last_updated      TEXT,
    vector_point_id   INTEGER
);

CREATE TABLE IF NOT EXISTS story_company (
    story_id   INTEGER NOT NULL REFERENCES story(id),
    company_id INTEGER NOT NULL REFERENCES company(id),
    is_primary INTEGER NOT NULL DEFAULT 0,
    UNIQUE (story_id, company_id)
);

CREATE TABLE IF NOT EXISTS story_relation (
    story_id          INTEGER NOT NULL REFERENCES story(id),
    related_story_id  INTEGER NOT NULL REFERENCES story(id),
    relation          TEXT,
    note              TEXT,
    UNIQUE (story_id, related_story_id)
);

CREATE TABLE IF NOT EXISTS article (
    id              INTEGER PRIMARY KEY,
    story_id        INTEGER NOT NULL REFERENCES story(id),
    url_canonical   TEXT NOT NULL UNIQUE,
    url_original    TEXT,
    title           TEXT,
    outlet          TEXT,
    source_id       TEXT,
    guid            TEXT,
    published_at    TEXT,
    fetched_at      TEXT,
    excerpt         TEXT
);

CREATE INDEX IF NOT EXISTS article_story ON article (story_id);
CREATE INDEX IF NOT EXISTS article_published ON article (published_at);

CREATE TABLE IF NOT EXISTS digest (
    id          INTEGER PRIMARY KEY,
    digest_date TEXT NOT NULL UNIQUE,
    intro_md    TEXT
);

CREATE TABLE IF NOT EXISTS digest_entry (
    digest_id  INTEGER NOT NULL REFERENCES digest(id),
    story_id   INTEGER NOT NULL REFERENCES story(id),
    kind       TEXT NOT NULL,
    delta_md   TEXT,
    position   INTEGER NOT NULL DEFAULT 0,
    UNIQUE (digest_id, story_id)
);

CREATE INDEX IF NOT EXISTS digest_entry_story ON digest_entry (story_id);

-- The day's coverage, as against the day's stories. digest_entry says which
-- stories a day touched; this says which articles arrived to touch them, and it
-- is the only link from a digest back to the material it was built from.
--
-- Not derivable from article.fetched_at, which answers a different question.
-- fetched_at is when we first saw a URL: an article can be fetched on one day
-- and only reach a digest on another, an article already in the corpus can be
-- named again by a later day's coverage, and a rebuilt corpus stamps every row
-- with the day it was rebuilt. This table records what a given digest actually
-- gathered, and survives all three.
CREATE TABLE IF NOT EXISTS digest_article (
    digest_id  INTEGER NOT NULL REFERENCES digest(id),
    article_id INTEGER NOT NULL REFERENCES article(id),
    UNIQUE (digest_id, article_id)
);

CREATE INDEX IF NOT EXISTS digest_article_article ON digest_article (article_id);

-- Seed categories. Idempotent: only insert a slug that is not already present.
INSERT OR IGNORE INTO category (slug, label) VALUES ('model',          'Model');
INSERT OR IGNORE INTO category (slug, label) VALUES ('regulation',     'Regulation');
INSERT OR IGNORE INTO category (slug, label) VALUES ('business',       'Business');
INSERT OR IGNORE INTO category (slug, label) VALUES ('pricing',        'Pricing');
INSERT OR IGNORE INTO category (slug, label) VALUES ('general_tech',   'General Tech');
