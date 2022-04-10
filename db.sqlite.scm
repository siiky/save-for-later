(module sfl.db.sqlite
  (
   call-with-database
   close
   open
   with-database

   run-schema
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
      ((with-database path db expr ...)
       (call-with-database path (lambda (db) expr ...)))))

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
  (defsql (node/remove-pins node) #!sql"sql/node.remove-pins.sql" fetch-all #:node node)
  (defsql (node/unpin-entry node cid) #!sql"sql/node.unpin-entry.sql" fetch #:node node #:cid cid)

  (defsql (type/add name) #!sql"sql/type.add.sql" fetch #:name name)
  (defsql (type/remove name) #!sql"sql/type.remove.sql" fetch #:name name)
  (defsql (type/remove name) #!sql"sql/type.remove.sql" fetch #:name name)

  (defsql (entry/add cid name url type) #!sql"sql/entry.add.sql" fetch #:cid cid #:name name #:url url #:type type)
  (defsql (entry/change-name cid name) #!sql"sql/entry.change-name.sql" fetch #:cid cid #:name name)
  (defsql (entry/remove cid) #!sql"sql/entry.remove.sql" fetch #:cid cid)
  (defsql (entry/set-consumed cid consumed) #!sql"sql/entry.set-consumed.sql" fetch #:cid cid #:consumed consumed)

  (defsql (pin/add node cid) #!sql"sql/pin.add.sql" fetch #:node node #:cid cid)
  (defsql (pin/remove cid) #!sql"sql/pin.remove.sql" fetch #:cid cid)

  (define (run-schema db)
    (let ((lst (map (cute <> db)
                    `(,schema/nodes ,schema/types ,schema/entries ,schema/pins))))
      (lambda ()
        (=> lst
            (map (cute <>) _)
            (filter (complement null?) _)))))
  )
