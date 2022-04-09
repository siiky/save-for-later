all: sfl

sfl: sql-reader-syntax/sql-reader-syntax.scm
	chicken-install -n

install: all
	chicken-install

test: install
	chicken-install -n -test

.PHONY: all install sfl test
