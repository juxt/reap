.PHONY: all test lint lint2 watch clean

all: 	lint2 test

test:
	clojure -Atest && echo "reap:PASS" > /tmp/reap-test-status || echo "<span foreground='red'>reap:FAIL</span>" > /tmp/reap-test-status

lint:
	clojure -Alint

lint2:
	clj-kondo --lint src/juxt --lint test/juxt

watch:
	find . -name "*.clj" | entr make test

clean:
	rm /tmp/reap-test-status
