;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.rfc7231-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.rfc7231 :as rfc7231]
   [juxt.reap.regex :as re]))

(deftest subtype-test
  (is (= "html" (re-matches (re-pattern rfc7231/subtype) "html"))))

(deftest parameter-test
  (testing "value"
    (is
     (=
      {:name "foo", :value "bar"}
      ((rfc7231/parameter {})
       (re/input "foo=bar"))))
    (is
     (nil?
      ((rfc7231/parameter {})
       (re/input "foo")))))

  (testing "quoted-string"
    (is
     (=
      {:name "foo", :value "ba'r" :raw-value "\"ba\\'r\""}
      ((rfc7231/parameter)
       (re/input "foo=\"ba\\'r\"")))))

  (testing "optional parameter"
    (is
     (=
      {:name "foo"}
      ((rfc7231/optional-parameter)
       (re/input "foo"))))
    (is
     (=
      {:name "foo" :value "bar"}
      ((rfc7231/optional-parameter)
       (re/input "foo=bar"))))
    (is
     (=
      {:name "foo", :value "ba'r" :raw-value "\"ba\\'r\""}
      ((rfc7231/optional-parameter)
       (re/input "foo=\"ba\\'r\""))))))

(deftest media-range-test
  (is
   (=
    [:media-range
     {:match "text/html",
      :type "text",
      :subtype "html",
      :params [{:name "foo", :value "bar"} {:name "baz", :value "qu'x" :raw-value "\"qu\\'x\""}]}]
    ((rfc7231/media-range)
     (re/input "text/html;foo=bar;baz=\"qu\\'x\"")))))


(deftest qvalue-test
  (is (re-matches (re-pattern rfc7231/qvalue) "1"))
  (is (re-matches (re-pattern rfc7231/qvalue) "1.000"))
  (is (re-matches (re-pattern rfc7231/qvalue) "0.9"))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "1.001")))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "1.0000")))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "0.1234"))))

;; TODO: weight tests

(deftest accept-test
  (is
   (=
    [{:match "text/html"
      :type "text",
      :subtype "html",
      :parameters [{:name "foo", :value "bar"}],
      :weight (float 0.3)
      :accept-ext [{:name "zip"} {:name "qux", :value "quik"}]}]
    ((rfc7231/accept)
     (re/input "text/html ;   foo=bar ;q=0.3;zip;\t qux=quik"))))

  ;; https://www.newmediacampaigns.com/blog/browser-rest-http-accept-headers
  (testing "Firefox"
    (is
     ((rfc7231/accept)
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"))))

  (testing "Webkit"
    (is
     ((rfc7231/accept)
      (re/input "application/xml,application/xhtml+xml,text/html;q=0.9,\r\ntext/plain;q=0.8,image/png,*/*;q=0.5"))))

  (testing "IE"
    (is
     ((rfc7231/accept)
      (re/input "image/jpeg, application/x-ms-application, image/gif,\r\napplication/xaml+xml, image/pjpeg, application/x-ms-xbap,\r\napplication/x-shockwave-flash, application/msword, */*"))))

  (testing "Windows 7 Chrome"
    (is
     ((rfc7231/accept)
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9")))))
