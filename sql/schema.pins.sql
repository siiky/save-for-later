CREATE TABLE pins (
    node TEXT NOT NULL REFERENCES nodes (id),
    cid  TEXT NOT NULL REFERENCES entries (cid)
);
