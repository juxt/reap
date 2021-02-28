;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7230-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7230 :refer [host OWS quoted-string token]]))

(alias 'rfc7230 (create-ns 'juxt.reap.alpha.rfc7230))

(deftest ows-test
  (let [p (re-pattern OWS)]
    (is (re-matches p " "))
    (is (re-matches p "\t"))
    (is (re-matches p "\t  "))
    (is (not (re-matches p "\r\n")))))

(deftest quoted-string-test
  (let [p (re-pattern quoted-string)]
    (is (not (re-matches p "abc")))
    (is (re-matches p "\"abc\""))
    (testing "embedded quotes"
      (is (re-matches p "\"ab\\\"c\"")))))

(deftest token-test
  (is (= "hjellowor909ld!" (re-matches (re-pattern token) "hjellowor909ld!")))
  (is (nil? (re-matches (re-pattern token) "hjello wor909ld!"))))


(deftest host-test
  (is (= #::rfc7230{:uri-host "ab%2Fc.def.d",
                    :port 23,
                    :host "ab%2Fc.def.d:23",
                    :decoded-host "ab/c.def.d:23"}
         ((host {}) (re/input "ab%2Fc.def.d:23"))))

  (is (= #::rfc7230{:uri-host "juxt.pro",
                    :host "juxt.pro",
                    :decoded-host "juxt.pro"} ((host {}) (re/input "juxt.pro"))))

  (is (= #::rfc7230{:uri-host "juxt.pro",
                    :port 2021,
                    :host "juxt.pro:2021",
                    :decoded-host "juxt.pro:2021"} ((host {}) (re/input "juxt.pro:2021"))))
  (is (thrown?
       Exception
       ((host {}) (re/input "juxt.pro:2000a"))))

  ;; "For example, values for Host and Origin MUST NOT be assumed to be free of
  ;; possibly malicious sequences such as /.. or others" --
  ;; https://solidproject.org/TR/protocol#security-considerations
  (is (thrown?
       Exception ((host {}) (re/input "juxt.pro/../"))))

  ;; Huge port numbers are valid (from a parsing perspective) but don't get
  ;; returned, nor do they cause an exception to be thrown.
  (testing "Huge port numbers"
    (is (= #::rfc7230{:uri-host "juxt.pro",
                      :host "juxt.pro",
                      :decoded-host "juxt.pro"} ((host {}) (re/input "juxt.pro:2021798798798"))))))
