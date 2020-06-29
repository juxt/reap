;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7231-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.regex :as re]))

(deftest subtype-test
  (is (= "html" (re-matches (re-pattern rfc7231/subtype) "html"))))

(deftest parameter-test
  (testing "value"
    (is
     (=
      #:juxt.reap.alpha{:parameter-name "foo", :parameter-value "bar"}
      ((:juxt.reap/decode (rfc7231/parameter {}))
       (re/input "foo=bar"))))
    (is
     (nil?
      ((:juxt.reap/decode (rfc7231/parameter {}))
       (re/input "foo")))))

  ;; "The type, subtype, and parameter name tokens are
  ;; case-insensitive. Parameter values might or might not be
  ;; case-sensitive, depending on the semantics of the parameter
  ;; name." -- RFC 7231 Section 3.1.1.1
  ;; TODO: Restore this test once options can be provided.
  #_(testing "case-insensitivity"
    (is
     (=
      ;; :parameter-name should be lower-case, but value unchanged.
      #:juxt.reap.alpha{:parameter-name "foo", :parameter-value "Bar"}
      ((rfc7231/parameter)
       (re/input "FOO=Bar"))))
    (is
     (nil?
      ((rfc7231/parameter)
       (re/input "foo")))))

  (testing "quoted-string"
    (is
     (=
      #:juxt.reap.alpha{:parameter-name "foo", :parameter-value "ba'r"}
      ((:juxt.reap/decode (rfc7231/parameter {}))
       (re/input "foo=\"ba\\'r\"")))))

  (testing "optional parameter"
    (is
     (=
      #:juxt.reap.alpha{:parameter-name "foo"}
      ((:juxt.reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo"))))
    (is
     (=
      #:juxt.reap.alpha{:parameter-name "foo" :parameter-value "bar"}
      ((:juxt.reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo=bar"))))
    (is
     (=
      #:juxt.reap.alpha{:parameter-name "foo", :parameter-value "ba'r"}
      ((:juxt.reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo=\"ba\\'r\""))))))

(deftest content-language-test
  (is
   (= ["en" "de"]
      (map :juxt.reap.alpha/language ((:juxt.reap/decode (rfc7231/content-language {})) (re/input "en,de"))))
   (= [["en" "US"] ["de" nil]]
      (map (juxt :juxt.reap.alpha/language :juxt.reap.alpha/region)))))

(deftest media-range-without-parameters-test
  (is
   (= #:juxt.reap.alpha
      {:media-range "text/*"
       :type "text"
       :subtype "*"}
      ((rfc7231/media-range-without-parameters {})
       (re/input "text/*")))))

(deftest media-range-test
  (is
   (=
    #:juxt.reap.alpha
    {:media-range "text/html"
     :type "text",
     :subtype "html",
     :parameter-map {"foo" "bar" "baz" "qu'x"}
     :parameters [#:juxt.reap.alpha{:parameter-name "FOO" :parameter-value "bar"}
                  #:juxt.reap.alpha{:parameter-name "Baz" :parameter-value "qu'x"}]}
    ((:juxt.reap/decode (rfc7231/media-range {}))
     (re/input "text/html;FOO=bar;Baz=\"qu\\'x\""))))

  ;; "The type, subtype, and parameter name tokens are
  ;; case-insensitive." -- RFC 7231 Section 3.1.1.1
  (testing "case insensitivity"
    (is
     (=
      #:juxt.reap.alpha
      {:media-range "TEXT/Html"
       :type "TEXT"
       :subtype "Html"
       :parameter-map {}
       :parameters []}
      ((:juxt.reap/decode (rfc7231/media-range {}))
       (re/input "TEXT/Html"))))))

(deftest qvalue-test
  (is (re-matches (re-pattern rfc7231/qvalue) "1"))
  (is (re-matches (re-pattern rfc7231/qvalue) "1.000"))
  (is (re-matches (re-pattern rfc7231/qvalue) "0.9"))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "1.001")))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "1.0000")))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "0.1234"))))

;; TODO: weight tests

(deftest media-type-test
  (is (= #:juxt.reap.alpha{:type "text" :subtype "html" :parameters [] :parameter-map {}}
         ((:juxt.reap/decode (rfc7231/media-type {}))
          (re/input "text/html"))))
  (is (= #:juxt.reap.alpha{:type "text"
                           :subtype "html"
                           :parameter-map {"foo" "bar" "zip" "qux"}
                           :parameters [#:juxt.reap.alpha{:parameter-name "foo" :parameter-value "bar"}
                                        #:juxt.reap.alpha{:parameter-name "ZIP" :parameter-value "qux"}]}
         ((:juxt.reap/decode (rfc7231/media-type {}))
          (re/input "text/html;foo=bar;ZIP=qux")))))

(deftest year-test
  (is (= "2020" ((rfc7231/year {}) (re/input "2020"))))
  (is (nil? ((rfc7231/year {}) (re/input "123")))))

;; TODO: Create a very cryptic Accept test designed to catch out all but the most compliant of parsers

((:juxt.reap/decode (rfc7231/accept {}))
 (re/input "text/html ;   foo=bar ;q=0.3;zip;\t qux=quik"))

((rfc7231/accept {})
 (re/input "text/html ;   foo=bar ;q=0.3;zip;\t qux=quik"))

(deftest accept-test
  (is
   (=
    [#:juxt.reap.alpha
     {:media-range "text/html"
      :type "text",
      :subtype "html",
      :parameters {"foo" "bar"}
      :qvalue 0.3
      :accept-ext
      [#:juxt.reap.alpha{:parameter-name "zip"}
       #:juxt.reap.alpha{:parameter-name "qux"
                         :parameter-value "quik"}]}]
    ((:juxt.reap/decode (rfc7231/accept {}))
     (re/input "text/html ;   foo=bar ;q=0.3;zip;\t qux=quik"))))

  (testing "Bad accept headers"
    (is
     (= '()
        ((:juxt.reap/decode (rfc7231/accept {}))
         (re/input "text"))))
    (is
     (= '()
        ((:juxt.reap/decode (rfc7231/accept {}))
         (re/input "text;text/html")))))

  ;; https://www.newmediacampaigns.com/blog/browser-rest-http-accept-headers
  (testing "Firefox"
    (is
     ((:juxt.reap/decode (rfc7231/accept {}))
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"))))

  (testing "Webkit"
    (is
     ((:juxt.reap/decode (rfc7231/accept {}))
      (re/input "application/xml,application/xhtml+xml,text/html;q=0.9,\r\ntext/plain;q=0.8,image/png,*/*;q=0.5"))))

  (testing "IE"
    (is
     ((:juxt.reap/decode (rfc7231/accept {}))
      (re/input "image/jpeg, application/x-ms-application, image/gif,\r\napplication/xaml+xml, image/pjpeg, application/x-ms-xbap,\r\napplication/x-shockwave-flash, application/msword, */*"))))

  (testing "Windows 7 Chrome"
    (is
     ((:juxt.reap/decode (rfc7231/accept {}))
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9")))))

(deftest accept-charset-test
  (is
   (= [#:juxt.reap.alpha{:charset "UTF-8", :qvalue 0.8}
       #:juxt.reap.alpha{:charset "shift_JIS", :qvalue 0.4}]
      ((:juxt.reap/decode (rfc7231/accept-charset {}))
       (re/input ", \t, , , UTF-8;q=0.8,shift_JIS;q=0.4")))))

(deftest accept-language-test
  (is
   (= [#:juxt.reap.alpha{:language-range "en-GB"}
       #:juxt.reap.alpha{:language-range "en-US" :qvalue 0.8}
       #:juxt.reap.alpha{:language-range "en" :qvalue 0.5}
       #:juxt.reap.alpha{:language-range "it" :qvalue 0.3}]
      ((:juxt.reap/decode (rfc7231/accept-language {}))
       (re/input "en-GB,en-US;q=0.8,en;q=0.5,it;q=0.3"))))

  (are [input expected]
      (= expected
         ((:juxt.reap/decode (rfc7231/accept-language {})) (re/input input)))

    ", , de ;q=0.7" [#:juxt.reap.alpha{:language-range "de" :qvalue 0.7}]

    "en-US ; q=1.0 ," [#:juxt.reap.alpha{:language-range "en-US", :qvalue 1.0}]

    "*;q=0.9 , fr;q=0.9" [#:juxt.reap.alpha{:language-range "*", :qvalue 0.9}
                          #:juxt.reap.alpha{:language-range "fr", :qvalue 0.9}]

    ", *" [#:juxt.reap.alpha{:language-range "*"}]

    ", , fr ;q=0.7" [#:juxt.reap.alpha{:language-range "fr", :qvalue 0.7}]

    "de;q=1.0" [#:juxt.reap.alpha{:language-range "de", :qvalue 1.0}]

    ", de;q=1.0" [#:juxt.reap.alpha{:language-range "de", :qvalue 1.0}]

    "en-US ;q=0.7 ," [#:juxt.reap.alpha{:language-range "en-US", :qvalue 0.7}]

    ", * ," [#:juxt.reap.alpha{:language-range "*"}]

    ", * ,en-US ;q=0.7 , *"
    [#:juxt.reap.alpha{:language-range "*"}
     #:juxt.reap.alpha{:language-range "en-US", :qvalue 0.7}
     #:juxt.reap.alpha{:language-range "*"}]))

(deftest accept-encoding-test
  (is
   (= [#:juxt.reap.alpha{:codings "gzip" :qvalue 0.3}
       #:juxt.reap.alpha{:codings "deflate"}
       #:juxt.reap.alpha{:codings "br"}]
      ((:juxt.reap/decode (rfc7231/accept-encoding {}))
       (re/input "gzip;q=0.3, deflate, br"))))
  (is (= '()
         ((:juxt.reap/decode (rfc7231/accept-encoding {}))
          (re/input "")))))

(deftest content-encoding-test
  (is
   (= [#:juxt.reap.alpha{:content-coding "gzip"}
       #:juxt.reap.alpha{:content-coding "deflate"}]
      ((:juxt.reap/decode (rfc7231/content-encoding {}))
       (re/input "gzip,deflate"))))
  (is
   (= []
      ((:juxt.reap/decode (rfc7231/content-encoding {}))
       (re/input "")))))
