(module sql-reader-syntax ()
  (import scheme chicken.base chicken.io chicken.read-syntax chicken.syntax)
  (set-read-syntax! 'sql
    (lambda (port)
      (let ((path (read port)))
        (unless (string? path)
          (syntax-error "The #!sql syntax expects a string"))

        (let ((sql-stmt (call-with-input-file path (cute read-string #f <>) #:text)))
          (unless (string? sql-stmt)
            (syntax-error "Failed reading the SQL file"))
          sql-stmt)))))
