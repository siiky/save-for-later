(import chicken.module)

(import
  sfl
  (only sfl.db.sqlite with-database)
  )

(for-each
  print
  (with-database
    "cenas.sqlite3"
    db
    (list-types db)
    ))
