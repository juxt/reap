;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.regex-test
  (:require
   [juxt.reap.regex :as re]
   [clojure.test :refer [deftest is]]))

(deftest int->regex-test
  (is (= "\\x24" (re/int->regex (int \$))))
  (is (= "\\u2282" (re/int->regex (int \⊂)))))

(deftest re-concat-test
  (is (= "abc" (re/re-concat "a" "b" "c")))
  (is (= "a\\x24\\n" (re/re-concat \a \$ \newline))))
