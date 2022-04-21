(module sfl
  (
   add
   add-entry
   add-type
   current-node
   list-pinned-cids
   list-types
   pin-entry
   prepare-database
   remove
   )

  (import
    scheme
    chicken.base)

  (import (prefix ipfs |ipfs:|))

  (import (prefix sfl.db.sqlite |db:|))

  (define (current-node #!optional node)
    (or node (alist-ref 'ID (ipfs:id))))

  (define prepare-database db:prepare-database)

  (define (add-entry db cid name url type #!key (pin #t) node)
    (let ((node (current-node node)))
      (db:with-transaction
        db
        (lambda ()
          (when pin (pin-entry db cid node))
          ((db:add-entry db) cid name url type)))))

  (define (pin-entry db cid #!optional node)
    ((db:pin-to-node db) cid (current-node node)))

  (define (add db path
               #!key
               (name #f)
               (url #f)
               (pin #t)
               (raw-leaves #t)
               (trickle #f)
               (cid-version 1)
               (node #f)
               (type #f)
               )
    (let ((node (current-node node))
          (cid (alist-ref
                 'Hash
                 (ipfs:add
                   #:reader ipfs:reader/json
                   ; TODO: Accept files as well as directories.
                   #:writer (ipfs:writer/filesystem path)
                   #:pin pin
                   #:raw-leaves raw-leaves
                   #:trickle trickle
                   #:cid-version cid-version
                   ))))
      (add-entry db cid name url type #:node node #:pin pin)))

  (define (list-pinned-cids db #!key node all-nodes?)
    ((db:list-pinned-cids db)
     (and (not all-nodes?)
          (current-node node))))

  (define (remove db cid #!key (node #f))
    (let ((node (current-node node))
          (unpin-from-node (db:unpin-from-node db)))
      (unpin-from-node cid node)))

  (define (add-type db type)
    ((db:add-type db) type))

  (define list-types db:list-types)
  )
