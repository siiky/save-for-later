(import chicken.random)

(import cluckcheck test)

(import
  srfi-1
  srfi-41
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

(define-stream (gen-stream gen)
  (stream-cons (gen) (gen-stream gen)))

(define-stream (stream-take strm len)
  (if (positive? len)
      (stream-cons (stream-car strm) (stream-take (stream-cdr strm) (sub1 len)))
      stream-null))

(define (gen-list/len len gen)
  (=> (gen-stream gen)
      (stream-take _ len)
      (stream->list _)))

(define (gen-string/len len gen-char)
  (=> (gen-list/len len gen-char)
      (list->string _)))

(define (gen-id) (gen-string/len 52 gen-Alphanum))
(define (gen-cid) (gen-string/len 59 gen-alphanum)) ; bafyXXX-like CIDv1
(define (gen-name) (gen-string/len (pseudo-random-integer 11) gen-Alphanum))


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
