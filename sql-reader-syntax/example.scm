(import sql-de-lite)

(define-constant schema #!sql"schema.sql")
(define-constant data #!sql"data.sql")
(define-constant select #!sql"select.sql")

(print (call-with-database
         'memory
         (lambda (db)
           (let ((schema (sql db schema))
                 (data (sql db data))
                 (select (sql db select)))
             (query fetch-all schema)
             (query fetch-all data)
             (query fetch-all select)))))
