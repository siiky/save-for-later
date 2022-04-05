CREATE TABLE entries (
    cid      TEXT        PRIMARY KEY NOT NULL UNIQUE,
    name     TEXT        NOT NULL,
    consumed BOOLEAN     NOT NULL DEFAULT FALSE,
    url      TEXT        UNIQUE,
    type     VARCHAR(10) NOT NULL REFERENCES types (name)
);

CREATE TABLE nodes (
    id   TEXT        PRIMARY KEY NOT NULL UNIQUE,
    name VARCHAR(20) UNIQUE
);

CREATE TABLE pins (
    node TEXT NOT NULL REFERENCES nodes (id),
    cid  TEXT NOT NULL REFERENCES entries (cid)
);

CREATE TABLE types (
    name VARCHAR(10) PRIMARY KEY NOT NULL UNIQUE
);
