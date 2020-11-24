;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7232-test
  (:require
   [clojure.test :refer [deftest are]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7232 :as rfc7232]))

(deftest if-match-test
  (let [decode (rfc7232/if-match {})]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "\"xyzzy\"" [#:juxt.reap.alpha.rfc7232
                     {:entity-tag
                      {:weak? false
                       :opaque-tag "\"xyzzy\""}}]

        "\"xyzzy\", \t \"r2d2xxxx\", W/\"c3piozzzz\""
        [#:juxt.reap.alpha.rfc7232
         {:entity-tag
          {:weak? false
           :opaque-tag "\"xyzzy\""}}
         #:juxt.reap.alpha.rfc7232
         {:entity-tag
          {:weak? false
           :opaque-tag "\"r2d2xxxx\""}}
         #:juxt.reap.alpha.rfc7232
         {:entity-tag
          {:weak? true
           :opaque-tag "W/\"c3piozzzz\""}}]

        "*" #:juxt.reap.alpha.rfc7232{:wildcard "*"})))

(deftest if-none-match-test
  (let [decode (rfc7232/if-none-match {})]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "\"xyzzy\", \"r2d2xxxx\", W/\"c3piozzzz\""
        [#:juxt.reap.alpha.rfc7232
         {:entity-tag
          {:weak? false
           :opaque-tag "\"xyzzy\""}}
         #:juxt.reap.alpha.rfc7232
         {:entity-tag
          {:weak? false
           :opaque-tag "\"r2d2xxxx\""}}
         #:juxt.reap.alpha.rfc7232
         {:entity-tag
          {:weak? true
           :opaque-tag "W/\"c3piozzzz\""}}]

        "*" #:juxt.reap.alpha.rfc7232
        {:wildcard "*"})))
