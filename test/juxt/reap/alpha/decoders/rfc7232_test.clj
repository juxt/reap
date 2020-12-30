;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7232-test
  (:require
   [clojure.test :refer [deftest are]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7232 :as rfc7232]
   [juxt.reap.alpha.decoders.rfc7232 :as dec]))

(deftest if-match-test
  (let [decode (dec/if-match {})]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "\"xyzzy\"" [#::rfc7232
                     {:entity-tag
                      #::rfc7232
                      {:weak? false
                       :opaque-tag "\"xyzzy\""}}]

        "\"xyzzy\", \t \"r2d2xxxx\", W/\"c3piozzzz\""
        [#::rfc7232
         {:entity-tag
          #::rfc7232
          {:weak? false
           :opaque-tag "\"xyzzy\""}}
         #::rfc7232
         {:entity-tag
          #::rfc7232
          {:weak? false
           :opaque-tag "\"r2d2xxxx\""}}
         #::rfc7232
         {:entity-tag
          #::rfc7232
          {:weak? true
           :opaque-tag "\"c3piozzzz\""}}]

        "*" #::rfc7232{:wildcard "*"})))

(deftest if-none-match-test
  (let [decode (dec/if-none-match {})]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "\"xyzzy\", \"r2d2xxxx\", W/\"c3piozzzz\""
        [#::rfc7232
         {:entity-tag
          #::rfc7232
          {:weak? false
           :opaque-tag "\"xyzzy\""}}
         #::rfc7232
         {:entity-tag
          #::rfc7232
          {:weak? false
           :opaque-tag "\"r2d2xxxx\""}}
         #::rfc7232
         {:entity-tag
          #::rfc7232
          {:weak? true
           :opaque-tag "\"c3piozzzz\""}}]

        "*" #::rfc7232
        {:wildcard "*"})))
