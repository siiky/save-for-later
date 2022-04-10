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

  (test-group "running the schema"
    (test "Running the schema in clean DB succeeds" '() (dbtest db)))

  (test-group "node operations"
    (do-times
      100
      (let ((id (gen-id))
            (name (gen-name)))
        (test-group (string-append name ":" id)
          (dbtest
            db
            (test "Adding succeeds" '() ((node/add db) id name))
            (test-assert "Added node is listed" (elem? `(,id ,name) ((node/list db))))
            (test "Removing succeeds" '() ((node/remove db) id))
            (test-assert "Removed node is not listed" (!elem? `(,id ,name) ((node/list db))))
            )))))

  (test-group "type operations"
    (do-times
      100
      (let ((name (gen-name)))
        (test-group name
          (dbtest
            db
            (test "Adding succeeds" '() ((type/add db) name))
            (test-assert "Added type is listed" (elem? `(,name) ((type/list db))))
            (test "Removing succeeds" '() ((type/remove db) name))
            (test-assert "Removed type is not listed" (!elem? `(,name) ((type/list db))))
            )))))
  )

(test-exit)
