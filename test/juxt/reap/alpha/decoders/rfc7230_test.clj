;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7230-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7230 :as rfc7230]))

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


(deftest host-test
  (is (= {:host "ab%2Fc.def.d" :port 23} ((rfc7230/host {}) (re/input "ab%2Fc.def.d:23"))))
  (is (= {:host "juxt.pro"} ((rfc7230/host {}) (re/input "juxt.pro"))))
  (is (= {:host "juxt.pro" :port 2021} ((rfc7230/host {}) (re/input "juxt.pro:2021"))))
  (is (thrown?
       Exception
       ((rfc7230/host {}) (re/input "juxt.pro:2000a"))))

  ;; "For example, values for Host and Origin MUST NOT be assumed to be free of
  ;; possibly malicious sequences such as /.. or others" --
  ;; https://solidproject.org/TR/protocol#security-considerations
  (is (thrown?
       Exception ((rfc7230/host {}) (re/input "juxt.pro/../"))))

  ;; Huge port numbers are valid (from a parsing perspective) but don't get
  ;; returned, nor do they cause an exception to be thrown.
  (testing "Huge port numbers"
    (is (= {:host "juxt.pro"} ((rfc7230/host {}) (re/input "juxt.pro:2021798798798"))))))
