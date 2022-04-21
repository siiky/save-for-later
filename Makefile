all: sfl

sfl: sql-reader-syntax/sql-reader-syntax.scm
	chicken-install -n

install: all
	chicken-install

test: install
	chicken-install -n -test

clean:
	chicken-clean

.PHONY: all install sfl test
