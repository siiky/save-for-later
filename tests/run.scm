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
(define (!elem-by? by x lst) (!elem? x (map by lst)))

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

(define-syntax test-add
  (syntax-rules ()
   ((test-add row /add-op /list-op)
    (begin
     (test "Adding succeeds" '() /add-op)
     (test-assert "Added row is listed" (elem? row /list-op))))))

(define-syntax test-remove
  (syntax-rules ()
   ((test-remove row /remove-op /list-op)
    (begin
     (test "Removing succeeds" '() /remove-op)
     (test-assert "Removed row is not listed" (!elem? row /list-op))))))

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
       (dbtest
         db
         (do-times
           100
           (let ((var gen-var) ...)
             (let ((row `(,col ...)))
               (test-group test-group-name
                 (test-add row ((/add db) add-arg ...) ((/list db)))
                 (test-remove row ((/remove db) remove-arg ...) ((/list db)))))))))
     )))

(define (gen-bool) (pseudo-random-integer 2))
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

  (test-group "entry operations"
    (dbtest
      db
      (do-times
        100
        (let ((cid (gen-cid))
              (name (gen-name))
              (new-name (gen-name))
              (consumed (gen-bool))
              (url (gen-string/len 40 gen-alphanum))
              (type (gen-name)))
          (let ((row `(,cid ,name 0 ,url ,type))
                (row/new-name `(,cid ,new-name 0 ,url ,type))
                (row/consumed `(,cid ,new-name ,consumed ,url ,type)))
            (test-group (string-append cid ":" name ":" url ":" type)
              (test-add row ((entry/add db) cid name url type) ((entry/list db)))

              (test "Changing name succeeds" '() ((entry/change-name db) cid new-name))
              (test-assert "Entry has new name" (elem? row/new-name ((entry/list db))))

              (test "Setting consumed succeeds" '() ((entry/set-consumed db) cid consumed))
              (test-assert "Entry has new consumed value" (elem? row/consumed ((entry/list db))))

              (test "Removing succeeds" '() ((entry/remove db) cid))
              (test-assert "Removed row is not listed" (!elem-by? car cid ((entry/list db))))
              ))))))

  (test-group "pin operations"
    (dbtest
      db
      (do-times
        100
        (let ((node (gen-id))
              (cid (gen-cid)))
          (let ((row `(,node ,cid)))
            (test-group (string-append node ":" cid)
              (test-add row ((pin/add db) node cid) ((pin/list db)))
              (test-remove row ((pin/remove-cid db) cid) ((pin/list db)))

              (test-add row ((pin/add db) node cid) ((pin/list db)))
              (test-remove row ((pin/remove-cid-from-node db) node cid) ((pin/list db)))

              (test-add row ((pin/add db) node cid) ((pin/list db)))
              (test-remove row ((pin/remove-node db) node) ((pin/list db)))
              ))))))
  )

(test-exit)
