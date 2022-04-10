(import chicken.random)

(import cluckcheck test)

(import
  srfi-1
  (rename
    (only srfi-197
          chain
          chain-lambda)
    (chain =>)
    (chain-lambda ->)))

(define (!elem? x lst) (not (member x lst equal?)))
(define (elem? x lst) (not (!elem? x lst)))

(define-syntax do-times
  (syntax-rules ()
    ((do-times times expr ...)
     (let loop ((times% times))
       (when (positive? times%)
         (begin expr ... (loop (sub1 times%))))))))

(define-syntax dbtest
  (syntax-rules ()
    ((dbtest db expr ...)
     (with-database 'memory db ((run-schema db)) expr ...))))

(define-syntax tbltest
  (syntax-rules ()
    ((tbltest thing                    ; The name of the thing to test
              ((var gen-var) ...)      ; The variables to generate and their respective generating expressions
              (col ...)                ; The expected columns from a /list operations
              (/add add-arg ...)       ; The /add operation and the required arguments (excluding the db)
              /list                    ; The /list operation
              (/remove remove-arg ...) ; Same as /add
              test-group-name)         ; The test-group name of a specific run

     (test-group (string-append thing " operations")
       (do-times
         100
         (let ((var gen-var) ...)
           (let ((row `(,col ...)))
             (test-group test-group-name
               (dbtest
                 db
                 (test "Adding succeeds" '() ((/add db) add-arg ...))
                 (test-assert (string-append "Added " thing " is listed") (elem? row ((/list db))))
                 (test "Removing succeeds" '() ((/remove db) remove-arg ...))
                 (test-assert (string-append "Removed " thing " is not listed") (!elem? row ((/list db))))
                 )))))))))


(define (gen-char charset)
  (=> charset
      (string-length _)
      (pseudo-random-integer _)
      (string-ref charset _)))

(define (gen-alphanum) (gen-char "0123456789abcdefghijklmnopqrstuvwxyz"))
(define (gen-Alphanum) (gen-char "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define ((gen-list/len len) gen)
  (let loop ((len len)
             (ret '()))
    (if (zero? len)
        ret
        (loop (sub1 len) (cons (gen) ret)))))

(define (gen-string/len gen-char len)
  (=> (gen-list/len len)
      (_ gen-char)
      (list->string _)))

(define (gen-id) (gen-string/len gen-Alphanum 52))
(define (gen-cid) (gen-string/len gen-alphanum 59)) ; CIDv1 bafyXXX-like
(define (gen-name) (gen-string/len gen-Alphanum (pseudo-random-integer 11)))


(test-group "sfl.db.sqlite"
  (import sfl.db.sqlite)

  (test-group "the schema"
    (test "Running the schema in clean DB succeeds" '() (dbtest db)))

  (tbltest "node"
           ((id (gen-id))
            (name (gen-name)))
           (id name)
           (node/add id name)
           node/list
           (node/remove id)
           (string-append name ":" id))

  (tbltest "type"
           ((name (gen-name)))
           (name)
           (type/add name)
           type/list
           (type/remove name)
           name)
  )

(test-exit)
