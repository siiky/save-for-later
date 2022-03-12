CREATE TABLE entries (
    /* TODO: How to deal with temporary lack of CID? */
    -- Primary key
    cid TEXT UNIQUE

    name TEXT NOT NULL,

    consumed BOOLEAN NOT NULL DEFAULT FALSE,

    url TEXT UNIQUE,

    type TINYINT NOT NULL,
);

CREATE TABLE nodes (
    id TEXT PRIMARY KEY NOT NULL UNIQUE
);

CREATE TABLE pins (
    node TEXT NOT NULL REFERENCES nodes (id),

    cid TEXT NOT NULL
);
