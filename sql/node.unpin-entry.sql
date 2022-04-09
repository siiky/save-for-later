DELETE FROM pins
WHERE node = :node
  AND cid = :cid;
