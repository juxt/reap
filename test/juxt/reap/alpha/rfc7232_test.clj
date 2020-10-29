;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7232-test
  (:require
   [clojure.test :refer [deftest are]]
   [juxt.reap.alpha :as reap]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7232 :as rfc7232]))

(deftest if-match-test
  (let [decode (::reap/decode (rfc7232/if-match {}))]
    (are [input expected]
        (= expected
           (decode (re/input input)))

      "\"xyzzy\"" [#::rfc7232
                   {:entity-tag
                    {:weak? false
                     :entity-tag "\"xyzzy\""}}]

      "\"xyzzy\", \t \"r2d2xxxx\", W/\"c3piozzzz\""
      [#::rfc7232{:entity-tag
                  {:weak? false
                   :entity-tag "\"xyzzy\""}}
       #::rfc7232{:entity-tag
                  {:weak? false
                   :entity-tag "\"r2d2xxxx\""}}
       #::rfc7232{:entity-tag
                  {:weak? true
                   :entity-tag "W/\"c3piozzzz\""}}]

      "*" #::rfc7232{:wildcard "*"})))

(deftest if-none-match-test
  (let [decode (::reap/decode (rfc7232/if-none-match {}))]
    (are [input expected]
        (= expected
           (decode (re/input input)))

      "\"xyzzy\", \"r2d2xxxx\", W/\"c3piozzzz\""
      [#::rfc7232{:entity-tag
                  {:weak? false
                   :entity-tag "\"xyzzy\""}}
       #::rfc7232{:entity-tag
                  {:weak? false
                   :entity-tag "\"r2d2xxxx\""}}
       #::rfc7232{:entity-tag
                  {:weak? true
                   :entity-tag "W/\"c3piozzzz\""}}]

      "*" #::rfc7232{:wildcard "*"})))
