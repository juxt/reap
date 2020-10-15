;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7232-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha.rfc7232 :as rfc7232]
   [juxt.reap.alpha.regex :as re]))

(deftest if-match-test
  (let [decode (:juxt.reap/decode (rfc7232/if-match {}))]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "\"xyzzy\"" [#:juxt.http
                     {:entity-tag
                      {:weak false
                       :tag "xyzzy"
                       :opaque-tag "\"xyzzy\""}}]

        "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\""
        [#:juxt.http{:entity-tag
                     {:weak false
                      :tag "xyzzy"
                      :opaque-tag "\"xyzzy\""}}
         #:juxt.http{:entity-tag
                     {:weak false
                      :tag "r2d2xxxx"
                      :opaque-tag "\"r2d2xxxx\""}}
         #:juxt.http{:entity-tag
                     {:weak false
                      :tag "c3piozzzz"
                      :opaque-tag "\"c3piozzzz\""}}]

        "*" #:juxt.http{:wildcard "*"})))

(deftest if-none-match-test
  (let [decode (:juxt.reap/decode (rfc7232/if-none-match {}))]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "W/\"xyzzy\", W/\"r2d2xxxx\", W/\"c3piozzzz\""
        [#:juxt.http{:entity-tag
                     {:weak true
                      :tag "xyzzy"
                      :opaque-tag "\"xyzzy\""}}
         #:juxt.http{:entity-tag
                     {:weak true
                      :tag "r2d2xxxx"
                      :opaque-tag "\"r2d2xxxx\""}}
         #:juxt.http{:entity-tag
                     {:weak true
                      :tag "c3piozzzz"
                      :opaque-tag "\"c3piozzzz\""}}]

        "*" #:juxt.http{:wildcard "*"})))
