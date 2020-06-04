.PHONY: all test lint lint2 watch

all: 	lint2 test

test:
	clojure -Atest

lint:
	clojure -Alint

lint2:
	clj-kondo --lint src/juxt --lint test/juxt

watch:
	find . -name "*.clj" | entr make test
