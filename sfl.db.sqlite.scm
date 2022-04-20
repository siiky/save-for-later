(module sfl.db.sqlite
  (
   call-with-database
   close
   open
   with-database

   run-schema

   add-entry
   add-type
   list-pinned-cids
   not-pinned-to-node?
   pin-to-node
   pinned-to-node?
   prepare-database
   unpin-from-node

   with-transaction
   )

  (import
    scheme
    chicken.base
    chicken.module
    )

  (import
    srfi-1
    (rename
      (only srfi-197
            chain
            chain-lambda)
      (chain =>)
      (chain-lambda ->)))

  (import sql-de-lite)

  (define open open-database)
  (define close close-database)

  (define-syntax with-database
    (syntax-rules ()
      ((with-database filename db expr ...)
       (call-with-database filename (lambda (db) expr ...)))))

  (define-syntax defsql
    (syntax-rules ()
      ((defsql name sqlstr)
       (defsql name sqlstr fetch-all))

      ((defsql (name param ...) sqlstr fetcher arg ...)
       (begin
         ; TODO: Don't automatically export.
         (export name)
         (define (name db)
           (let ((stmt (sql db sqlstr)))
             (lambda (param ...)
               (query fetcher stmt arg ...))))))))

  ;; Base DB operations

  (defsql (schema/nodes) #!sql"sql/schema.nodes.sql")
  (defsql (schema/types) #!sql"sql/schema.types.sql")
  (defsql (schema/entries) #!sql"sql/schema.entries.sql")
  (defsql (schema/pins) #!sql"sql/schema.pins.sql")

  (defsql (node/add id name) #!sql"sql/node.add.sql" fetch #:id id #:name name)
  (defsql (node/list) #!sql"sql/node.list.sql")
  (defsql (node/remove id) #!sql"sql/node.remove.sql" fetch #:id id)

  (defsql (type/add name) #!sql"sql/type.add.sql" fetch #:name name)
  (defsql (type/list) #!sql"sql/type.list.sql")
  (defsql (type/remove name) #!sql"sql/type.remove.sql" fetch #:name name)

  (defsql (entry/add cid name url type) #!sql"sql/entry.add.sql" fetch #:cid cid #:name name #:url url #:type type)
  (defsql (entry/change-name cid name) #!sql"sql/entry.change-name.sql" fetch #:cid cid #:name name)
  (defsql (entry/list) #!sql"sql/entry.list.sql")
  (defsql (entry/remove cid) #!sql"sql/entry.remove.sql" fetch #:cid cid)
  (defsql (entry/set-consumed cid consumed) #!sql"sql/entry.set-consumed.sql" fetch #:cid cid #:consumed consumed)

  (defsql (pin/add node cid) #!sql"sql/pin.add.sql" fetch #:node node #:cid cid)
  (defsql (pin/list) #!sql"sql/pin.list.sql")
  (defsql (pin/list-cids) #!sql"sql/pin.list-cids.sql" fetch-column)
  (defsql (pin/list-node node) #!sql"sql/pin.list-node.sql" fetch-column #:node node)
  (defsql (pin/pinned-to-node cid node) #!sql"sql/pin.pinned-to-node.sql" fetch-all #:node node #:cid cid)
  (defsql (pin/remove-cid cid) #!sql"sql/pin.remove-cid.sql" fetch #:cid cid)
  (defsql (pin/remove-cid-from-node cid node) #!sql"sql/pin.remove-cid-from-node.sql" fetch #:cid cid #:node node)
  (defsql (pin/remove-node node) #!sql"sql/pin.remove-node.sql" fetch-all #:node node)

  (define (run-schema db)
    (let ((lst (map (cute <> db)
                    `(,schema/nodes ,schema/types ,schema/entries ,schema/pins))))
      (lambda ()
        (=> lst
            (map (cute <>) _)
            (filter (o not null?) _)))))

  ;; High-level API

  (define (prepare-database db)
    (when (null? (schema db))
      ((run-schema db))))

  (define ((not-pinned-to-node? db) cid node)
    (null? ((pin/pinned-to-node db) cid node)))

  (define ((pinned-to-node? db) cid node)
    (not ((not-pinned-to-node? db) cid node)))

  (define ((pin-to-node db) cid node)
    (when ((not-pinned-to-node? db) cid node)
      ((pin/add db) node cid)))

  (define ((list-pinned-cids db) node-id)
    (if node-id
        ((pin/list-node db) node-id)
        ((pin/list-cids db))))

  (define unpin-from-node pin/remove-cid-from-node)

  (define add-type type/add)
  (define add-entry entry/add)
  )
