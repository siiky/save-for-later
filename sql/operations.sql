/*
 * Add a node
 */
INSERT INTO nodes (id) VALUES
(:id)
;

/*
 * Remove a node
 *
 * NOTE: This should be a transaction. The query is different whether it's for
 *       plain SQLite or rqlite.
 *
 * Make sure there are no unconsumed entries pinned to this node only.
 */
DELETE FROM pins WHERE node = :node;
DELETE FROM nodes WHERE id = :node;

/*
 * Add an entry
 */
INSERT INTO entries (cid, name, url, type) VALUES
(:cid, :name, :url, :type)
;

/*
 * Remove an entry
 *
 * See the NOTE for removing a node.
 */
DELETE FROM pins WHERE cid = :cid;
DELETE FROM entries WHERE cid = :cid;

/*
 * Mark an entry as consumed/not consumed
 */
UPDATE entries
SET consumed = TRUE -- or FALSE
WHERE cid = :cid;

/*
 * Pin an entry to some node(s)
 */
INSERT INTO pins (node, cid) VALUES
(:node, :cid)
;

/*
 * Unpin an entry from some node(s)
 */
DELETE FROM pins
WHERE node = :node AND cid = :cid;

/*
 * Change an entry's name
 */
UPDATE entries
SET name = :name
WHERE cid = :cid;

/*
 * Add a type
 */
INSERT INTO types (name) VALUES
(:name)
;

/*
 * Remove a type
 *
 * Make sure there are no entries of this type
 */
DELETE FROM types
WHERE name = :name;
