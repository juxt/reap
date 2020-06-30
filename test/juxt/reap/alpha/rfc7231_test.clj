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
      #:juxt.http{:parameter-name "foo", :parameter-value "bar"}
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
  (testing "case-insensitivity"
    (is
     (=
      ;; :parameter-name should be lower-case, but value unchanged.
      #:juxt.http{:parameter-name "foo", :parameter-value "Bar"}
      ((:juxt.reap/decode (rfc7231/parameter {}))
       (re/input "FOO=Bar"))))
    (is
     (nil?
      ((:juxt.reap/decode (rfc7231/parameter {}))
       (re/input "foo")))))

  (testing "quoted-string"
    (is
     (=
      #:juxt.http{:parameter-name "foo", :parameter-value "ba'r"}
      ((:juxt.reap/decode (rfc7231/parameter {}))
       (re/input "foo=\"ba\\'r\"")))))

  (testing "optional parameter"
    (is
     (=
      #:juxt.http{:parameter-name "foo"}
      ((:juxt.reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo"))))
    (is
     (=
      #:juxt.http{:parameter-name "foo" :parameter-value "bar"}
      ((:juxt.reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo=bar"))))
    (is
     (=
      #:juxt.http{:parameter-name "foo", :parameter-value "ba'r"}
      ((:juxt.reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo=\"ba\\'r\""))))))

(deftest content-language-test
  (is
   (= ["en" "de"]
      (map :juxt.http/language ((:juxt.reap/decode (rfc7231/content-language {})) (re/input "en,de"))))
   (= [["en" "US"] ["de" nil]]
      (map (juxt :juxt.http/language :juxt.http/region)))))

(deftest media-range-without-parameters-test
  (is
   (= #:juxt.http
      {:media-range "text/*"
       :type "text"
       :subtype "*"}
      ((rfc7231/media-range-without-parameters {})
       (re/input "text/*")))))

(deftest media-range-test
  (is
   (=
    #:juxt.http
    {:media-range "text/html"
     :type "text",
     :subtype "html",
     :parameter-map {"foo" "bar" "baz" "qu'x"}
     :parameters [#:juxt.http{:parameter-name "FOO" :parameter-value "bar"}
                  #:juxt.http{:parameter-name "Baz" :parameter-value "qu'x"}]}
    ((:juxt.reap/decode
      (rfc7231/media-range
       {:juxt.reap/decode-preserve-case true}))
     (re/input "text/html;FOO=bar;Baz=\"qu\\'x\""))))

  ;; "The type, subtype, and parameter name tokens are
  ;; case-insensitive." -- RFC 7231 Section 3.1.1.1
  (testing "case insensitivity"
    (is
     (=
      #:juxt.http
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

(deftest weight-test
  (are [input expected]
      (= expected ((:juxt.reap/decode (rfc7231/weight {})) (re/input input)))
    ";Q=0.9" 0.9
    ";q=0.9" 0.9
    ";q=0.000" 0.0
    ";q=1" 1.0
    ";q=1.0" 1.0
    ";q=1.00" 1.0
    ";q=1.000" 1.0
    ;; These are invalid, there are more than 3 decimal places.
    ";q=0.0000" nil
    ";q=1.0000" nil
    ";q=0.1234" nil
    ;; These are invalid, they are more than 1
    ";q=1.1" nil
    ";q=1.0001" nil))

(deftest media-type-test
  (is (= #:juxt.http{:type "text" :subtype "html" :parameters [] :parameter-map {}}
         ((:juxt.reap/decode
           (rfc7231/media-type {}))
          (re/input "text/html"))))
  (is (= #:juxt.http
         {:type "text"
          :subtype "html"
          :parameter-map {"foo" "bar" "zip" "qux"}
          :parameters [#:juxt.http
                       {:parameter-name "foo" :parameter-value "bar"}
                       #:juxt.http
                       {:parameter-name "ZIP" :parameter-value "qux"}]}
         ((:juxt.reap/decode
           (rfc7231/media-type
            {:juxt.reap/decode-preserve-case true}))
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
    [#:juxt.http
     {:media-range "text/html"
      :type "text",
      :subtype "html",
      :parameters {"foo" "bar"}
      :qvalue 0.3
      :accept-ext
      [#:juxt.http{:parameter-name "zip"}
       #:juxt.http{:parameter-name "qux"
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
   (= [#:juxt.http{:charset "UTF-8", :qvalue 0.8}
       #:juxt.http{:charset "shift_JIS", :qvalue 0.4}]
      ((:juxt.reap/decode (rfc7231/accept-charset {}))
       (re/input ", \t, , , UTF-8;q=0.8,shift_JIS;q=0.4")))))

(deftest accept-language-test
  (is
   (= [#:juxt.http{:language-range "en-GB"}
       #:juxt.http{:language-range "en-US" :qvalue 0.8}
       #:juxt.http{:language-range "en" :qvalue 0.5}
       #:juxt.http{:language-range "it" :qvalue 0.3}]
      ((:juxt.reap/decode (rfc7231/accept-language {}))
       (re/input "en-GB,en-US;q=0.8,en;q=0.5,it;q=0.3"))))

  (are [input expected]
      (= expected
         ((:juxt.reap/decode (rfc7231/accept-language {})) (re/input input)))

    ", , de ;q=0.7" [#:juxt.http{:language-range "de" :qvalue 0.7}]

    "en-US ; q=1.0 ," [#:juxt.http{:language-range "en-US", :qvalue 1.0}]

    "*;q=0.9 , fr;q=0.9" [#:juxt.http{:language-range "*", :qvalue 0.9}
                          #:juxt.http{:language-range "fr", :qvalue 0.9}]

    ", *" [#:juxt.http{:language-range "*"}]

    ", , fr ;q=0.7" [#:juxt.http{:language-range "fr", :qvalue 0.7}]

    "de;q=1.0" [#:juxt.http{:language-range "de", :qvalue 1.0}]

    ", de;q=1.0" [#:juxt.http{:language-range "de", :qvalue 1.0}]

    "en-US ;q=0.7 ," [#:juxt.http{:language-range "en-US", :qvalue 0.7}]

    ", * ," [#:juxt.http{:language-range "*"}]

    ", * ,en-US ;q=0.7 , *"
    [#:juxt.http{:language-range "*"}
     #:juxt.http{:language-range "en-US", :qvalue 0.7}
     #:juxt.http{:language-range "*"}]))

(deftest accept-encoding-test
  (is
   (= [#:juxt.http{:codings "gzip" :qvalue 0.3}
       #:juxt.http{:codings "deflate"}
       #:juxt.http{:codings "br"}]
      ((:juxt.reap/decode (rfc7231/accept-encoding {}))
       (re/input "gzip;q=0.3, deflate, br"))))
  (is (= '()
         ((:juxt.reap/decode (rfc7231/accept-encoding {}))
          (re/input "")))))

(deftest content-encoding-test
  (is
   (= [#:juxt.http{:content-coding "gzip"}
       #:juxt.http{:content-coding "deflate"}]
      ((:juxt.reap/decode (rfc7231/content-encoding {}))
       (re/input "gzip,deflate"))))
  (is
   (= []
      ((:juxt.reap/decode (rfc7231/content-encoding {}))
       (re/input "")))))
