;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.rfc7230-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.rfc7230 :as rfc7230]))

(deftest ows-test
  (let [OWS (re-pattern rfc7230/OWS)]
    (is (re-matches OWS " "))
    (is (re-matches OWS "\t"))
    (is (re-matches OWS "\t  "))
    (is (not (re-matches OWS "\r\n")))))

(deftest quoted-string-test
  (let [quoted-string (re-pattern rfc7230/quoted-string)]
    (is (not (re-matches quoted-string "abc")))
    (is (re-matches quoted-string "\"abc\""))
    (testing "embedded quotes"
      (is (re-matches quoted-string "\"ab\\\"c\"")))))

(deftest token-test
  (is (= "hjellowor909ld!" (re-matches (re-pattern rfc7230/token) "hjellowor909ld!")))
  (is (nil? (re-matches (re-pattern rfc7230/token) "hjello wor909ld!"))))
