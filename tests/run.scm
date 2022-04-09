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


(define gen-alphanum
  (let ((alphanum "0123456789abcdefghijklmnopqrstuvwxyz"))
    (lambda ()
      (=> alphanum
          (string-length _)
          (pseudo-random-integer _)
          (string-ref alphanum _)))))

(define ((gen-list/len len) gen)
  (let loop ((len len)
             (ret '()))
    (if (zero? len)
        ret
        (loop (sub1 len) (cons (gen) ret)))))

(define gen-string/len
  (-> (gen-list/len _)
      (_ gen-alphanum)
      (list->string _)))

(define (gen-id) (gen-string/len 52))
(define (gen-cid) (gen-string/len 59))
(define (gen-name) (gen-string/len (pseudo-random-integer 11)))


(test-group "sfl.db.sqlite"
  (import sfl.db.sqlite)
  (test "Schema doesn't error in clean DB" '() (dbtest db))

  (test-group "node operations"
    (do-times
      100
      (let ((id (gen-id))
            (name (gen-name)))
        (test-group (string-append name ":" id)
          (dbtest
            db
            (test "Adding succeeds" '() ((node/add db) id name))
            (test-assert "Added node is listed" (lset= equal? `((,id ,name)) ((node/list db))))
            (test "Removing works" '() ((node/remove db) id))
            (test-assert "Removed node is not listed" (not (member `(,id ,name) ((node/list db)))))
            )))))

  (test-group "type operations"
    (do-times
      100
      (let ((name (gen-name)))
        (test-group name
          (dbtest
            db
            (test "Adding succeeds" '() ((type/add db) name))
            )))))
  )
