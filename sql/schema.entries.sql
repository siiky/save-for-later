CREATE TABLE entries (
    cid      TEXT        PRIMARY KEY NOT NULL UNIQUE,
    name     TEXT        NOT NULL,
    consumed BOOLEAN     NOT NULL DEFAULT FALSE,
    url      TEXT        UNIQUE,
    type     VARCHAR(10) NOT NULL REFERENCES types (name)
);
