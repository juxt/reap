;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7231-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.decoders.rfc7231 :as dec]
   [juxt.reap.alpha.regex :as re]))

(deftest media-range-without-parameters-test
  (is
   (= #::rfc7231
      {:media-range "text/*"
       :type "text"
       :subtype "*"}
      ((dec/media-range-without-parameters {})
       (re/input "text/*")))))

(deftest accept-test
  (is
   (=
    [#::rfc7231
     {:media-range "text/html"
      :type "text",
      :subtype "html",
      :parameters {"foo" "bar"}
      :qvalue 0.3
      :accept-ext
      [#::rfc7231
       {:parameter-name "zip"}
       #::rfc7231
       {:parameter-name "qux"
        :parameter-value "quik"}]}]
    ((dec/accept {})
     (re/input "text/html ;   foo=bar ;q=0.3;zip;\t qux=quik"))))

  (testing "Bad accept headers"
    (is
     (nil?
        ((dec/accept {})
         (re/input "text"))))
    (is
     (nil?
      ((dec/accept {})
       (re/input "text;text/html")))))

  ;; https://www.newmediacampaigns.com/blog/browser-rest-http-accept-headers
  (testing "Firefox"
    (is
     ((dec/accept {})
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"))))

  (testing "Webkit"
    (is
     ((dec/accept {})
      (re/input "application/xml,application/xhtml+xml,text/html;q=0.9, text/plain;q=0.8,image/png,*/*;q=0.5"))))

  (testing "IE"
    (is
     ((dec/accept {})
      (re/input "image/jpeg, application/x-ms-application, image/gif, application/xaml+xml, image/pjpeg, application/x-ms-xbap, application/x-shockwave-flash, application/msword, */*"))))

  (testing "Windows 7 Chrome"
    (is
     ((dec/accept {})
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9")))))

;; TODO: Create a very cryptic Accept test designed to catch out all but the most compliant of parsers

(deftest accept-charset-test
  (is
   (= [#::rfc7231{:charset "UTF-8", :qvalue 0.8}
       #::rfc7231{:charset "shift_JIS", :qvalue 0.4}]
      ((dec/accept-charset {})
       (re/input ", \t, , , UTF-8;q=0.8,shift_JIS;q=0.4")))))

(deftest accept-encoding-test
  (is
   (= [#::rfc7231{:codings "gzip" :qvalue 0.3}
       #::rfc7231{:codings "deflate"}
       #::rfc7231{:codings "br"}]
      ((dec/accept-encoding {})
       (re/input "gzip;q=0.3, deflate, br"))))
  (is
   (nil?
    ((dec/accept-encoding {})
     (re/input "")))))

(deftest accept-language-test
  (is
   (= [{:juxt.reap.alpha.rfc4647/language-range "en-GB"}
       {:juxt.reap.alpha.rfc4647/language-range "en-US" ::rfc7231/qvalue 0.8}
       {:juxt.reap.alpha.rfc4647/language-range "en" ::rfc7231/qvalue 0.5}
       {:juxt.reap.alpha.rfc4647/language-range "it" ::rfc7231/qvalue 0.3}]
      ((dec/accept-language {})
       (re/input "en-GB,en-US;q=0.8,en;q=0.5,it;q=0.3"))))

  (are [input expected]
      (= expected
         ((dec/accept-language {}) (re/input input)))

    ", , de ;q=0.7" [{:juxt.reap.alpha.rfc4647/language-range "de" ::rfc7231/qvalue 0.7}]

    "en-US ; q=1.0 ," [{:juxt.reap.alpha.rfc4647/language-range "en-US", ::rfc7231/qvalue 1.0}]

    "*;q=0.9 , fr;q=0.9" [{:juxt.reap.alpha.rfc4647/language-range "*", ::rfc7231/qvalue 0.9}
                          {:juxt.reap.alpha.rfc4647/language-range "fr", ::rfc7231/qvalue 0.9}]

    ", *" [{:juxt.reap.alpha.rfc4647/language-range "*"}]

    ", , fr ;q=0.7" [{:juxt.reap.alpha.rfc4647/language-range "fr", ::rfc7231/qvalue 0.7}]

    "de;q=1.0" [{:juxt.reap.alpha.rfc4647/language-range "de", ::rfc7231/qvalue 1.0}]

    ", de;q=1.0" [{:juxt.reap.alpha.rfc4647/language-range "de", ::rfc7231/qvalue 1.0}]

    "en-US ;q=0.7 ," [{:juxt.reap.alpha.rfc4647/language-range "en-US", ::rfc7231/qvalue 0.7}]

    ", * ," [{:juxt.reap.alpha.rfc4647/language-range "*"}]

    ", * ,en-US ;q=0.7 , *"
    [{:juxt.reap.alpha.rfc4647/language-range "*"}
     {:juxt.reap.alpha.rfc4647/language-range "en-US", ::rfc7231/qvalue 0.7}
     {:juxt.reap.alpha.rfc4647/language-range "*"}]))

(deftest allow-test
  (is
   (= [#::rfc7231{:method "GET"}
       #::rfc7231{:method "HEAD"}
       #::rfc7231{:method "PUT"}]
      ((dec/allow {})
       (re/input "GET, HEAD, PUT")))))

(deftest content-encoding-test
  (is
   (= [#::rfc7231{:content-coding "gzip"}
       #::rfc7231{:content-coding "deflate"}]
      ((dec/content-encoding {})
       (re/input "gzip,deflate"))))
  (is
   (nil?
    ((dec/content-encoding {})
     (re/input "")))))

(deftest content-language-test
  (is
   (= ["en" "de"]
      (map :juxt.reap.alpha.rfc5646/language ((dec/content-language {}) (re/input "en,de"))))
   (= [["en" "US"] ["de" nil]]
      (map (juxt :juxt.reap.alpha.rfc5646/language ::rfc7231/region)))))

(deftest http-date-test
  (are [input expected]
      (= expected
         ((dec/http-date {})
          (re/input input)))
    "Sun, 06 Nov 1994 08:49:37 GMT"
    #::rfc7231
    {:imf-fixdate "Sun, 06 Nov 1994 08:49:37 GMT"
     :date #inst "1994-11-06T08:49:37.000-00:00"
     :day-name "Sun"
     :day "06"
     :month "Nov"
     :year "1994"
     :hour "08"
     :minute "49"
     :second "37"}

    "Sunday, 06-Nov-94 08:49:37 GMT"
    #::rfc7231
    {:rfc850-date "Sunday, 06-Nov-94 08:49:37 GMT"
     :day-name "Sunday"
     :day "06"
     :month "Nov"
     :year "94"
     :hour "08"
     :minute "49"
     :second "37"}

    "Sun Nov  6 08:49:37 1994"
    #::rfc7231
    {:asctime-date "Sun Nov  6 08:49:37 1994"
     :day-name "Sun"
     :day " 6"
     :month "Nov"
     :year "1994"
     :hour "08"
     :minute "49"
     :second "37"}))

(deftest imf-fixdate-test
  (let [decode (dec/imf-fixdate {})]
    (are [input expected]
        (= expected (decode (re/input input)))

      "Mon, 20 Jul 2020 12:00:00 GMT"
      #::rfc7231
      {:imf-fixdate "Mon, 20 Jul 2020 12:00:00 GMT"
       :date #inst "2020-07-20T12:00:00.000-00:00"
       :day-name "Mon"
       :day "20" :month "Jul" :year "2020"
       :hour "12" :minute "00" :second "00"}

      ;; Test bad input returns nil
      "Mon,20 Jul 2020 12:00:00 GMT" nil
      "Mon, 20 Jul 2020 12:00:00 BST" nil
      "Pie, 20 Jul 2020 12:00:00 GMT" nil)))

(deftest rfc850-date-test
  (let [decode (dec/rfc850-date {})]
    (are [input expected]
        (= expected (decode (re/input input)))

      "Sunday, 06-Nov-94 08:49:37 GMT"
      #::rfc7231
      {:rfc850-date "Sunday, 06-Nov-94 08:49:37 GMT"
       :day-name "Sunday"
       :day "06" :month "Nov" :year "94"
       :hour "08" :minute "49" :second "37"}

      ;; Test bad input returns nil
      "Sun, 06-Nov-94 08:49:37 GMT" nil
      "Sunday, 06-Nov-1994 08:49:37 GMT" nil
      "Sunday, 06-Nov-94 08:49:37 CET" nil)))

(deftest asctime-date-test
  (let [decode (dec/asctime-date {})]
    (are [input expected]
        (= expected (decode (re/input input)))
      "Sun Nov  6 08:49:37 1994"
      #::rfc7231
      {:asctime-date "Sun Nov  6 08:49:37 1994"
       :day-name "Sun"
       :day " 6"
       :month "Nov"
       :year "1994"
       :hour "08"
       :minute "49"
       :second "37"})))

(deftest codings-test
  ;; TODO
  )


#_;; ANSI C's format
((rfc7231/http-date {})
 (re/input "Sun Nov  6 08:49:37 1994"))


(deftest date1-test
  (is
   (=
    #::rfc7231
    {:day "30" :month "Sep" :year "2002"}
    ((dec/date1 {})
     (re/input "30 Sep 2002")))))

(deftest date2-test
  (is
   (=
    #::rfc7231
    {:day "10" :month "Apr" :year "82"}
    ((dec/date2 {})
     (re/input "10-Apr-82")))))

(deftest date3-test
  (is
   (=
    #::rfc7231
    {:day " 6" :month "Nov"}
    ((dec/date3 {})
     (re/input "Nov  6"))))
  (is
   (nil? ((dec/date3 {})
          (re/input "Nov 6"))))
  (is
   (=
    #::rfc7231
    {:day "12" :month "Jun"}
    ((dec/date3 {})
     (re/input "Jun 12")))))

(deftest media-range-test
  (is
   (=
    #::rfc7231
    {:media-range "text/html"
     :type "text",
     :subtype "html",
     :parameter-map {"foo" "bar" "baz" "qu'x"}
     :parameters [#::rfc7231{:parameter-name "FOO" :parameter-value "bar"}
                  #::rfc7231{:parameter-name "Baz" :parameter-value "qu'x"}]}
    ((dec/media-range
      {:juxt.reap.alpha/decode-preserve-case true})
     (re/input "text/html;FOO=bar;Baz=\"qu\\'x\""))))

  ;; "The type, subtype, and parameter name tokens are
  ;; case-insensitive." -- RFC 7231 Section 3.1.1.1
  (testing "case insensitivity"
    (is
     (=
      #::rfc7231
      {:media-range "TEXT/Html"
       :type "TEXT"
       :subtype "Html"
       :parameter-map {}
       :parameters []}
      ((dec/media-range {})
       (re/input "TEXT/Html"))))))

(deftest media-type-test
  (is (= #::rfc7231{:type "text" :subtype "html" :parameters [] :parameter-map {}}
         ((dec/media-type {})
          (re/input "text/html"))))
  (is (= #::rfc7231
         {:type "text"
          :subtype "html"
          :parameter-map {"foo" "bar" "zip" "qux"}
          :parameters [#::rfc7231{:parameter-name "foo" :parameter-value "bar"}
                       #::rfc7231{:parameter-name "ZIP" :parameter-value "qux"}]}
         ((dec/media-type
           {:juxt.reap.alpha/decode-preserve-case true})
          (re/input "text/html;foo=bar;ZIP=qux")))))

(deftest parameter-test
  (testing "value"
    (is
     (=
      #::rfc7231{:parameter-name "foo", :parameter-value "bar"}
      ((dec/parameter {})
       (re/input "foo=bar"))))
    (is
     (nil?
      ((dec/parameter {})
       (re/input "foo")))))

  ;; "The type, subtype, and parameter name tokens are
  ;; case-insensitive. Parameter values might or might not be
  ;; case-sensitive, depending on the semantics of the parameter
  ;; name." -- RFC 7231 Section 3.1.1.1
  (testing "case-insensitivity"
    (is
     (=
      ;; :parameter-name should be lower-case, but value unchanged.
      #::rfc7231{:parameter-name "foo", :parameter-value "Bar"}
      ((dec/parameter {})
       (re/input "FOO=Bar"))))
    (is
     (nil?
      ((dec/parameter {})
       (re/input "foo")))))

  (testing "quoted-string"
    (is
     (=
      #::rfc7231{:parameter-name "foo", :parameter-value "ba'r"}
      ((dec/parameter {})
       (re/input "foo=\"ba\\'r\"")))))

  (testing "optional parameter"
    (is
     (=
      #::rfc7231{:parameter-name "foo"}
      ((dec/optional-parameter {})
       (re/input "foo"))))
    (is
     (=
      #::rfc7231{:parameter-name "foo" :parameter-value "bar"}
      ((dec/optional-parameter {})
       (re/input "foo=bar"))))
    (is
     (=
      #::rfc7231{:parameter-name "foo", :parameter-value "ba'r"}
      ((dec/optional-parameter {})
       (re/input "foo=\"ba\\'r\""))))))

(deftest product-test
  ;; TODO
  )

(deftest product-version-test
  ;; TODO
  )

(deftest retry-after-test
  (are [input expected]
      (= expected
         ((dec/retry-after {})
          (re/input input)))

    "Wed, 29 Jul 2020 10:00:00 GMT"
    #::rfc7231
    {:day "29"
     :date #inst "2020-07-29T10:00:00.000-00:00"
     :hour "10"
     :second "00"
     :imf-fixdate "Wed, 29 Jul 2020 10:00:00 GMT"
     :month "Jul"
     :day-name "Wed"
     :year "2020"
     :minute "00"}

    "120"
    #::rfc7231
    {:delay-seconds "120"}))

(deftest qvalue-test
  (is (re-matches (re-pattern dec/qvalue) "1"))
  (is (re-matches (re-pattern dec/qvalue) "1.000"))
  (is (re-matches (re-pattern dec/qvalue) "0.9"))
  (is (nil? (re-matches (re-pattern dec/qvalue) "1.001")))
  (is (nil? (re-matches (re-pattern dec/qvalue) "1.0000")))
  (is (nil? (re-matches (re-pattern dec/qvalue) "0.1234"))))

(deftest subtype-test
  (is (= "html" (re-matches (re-pattern dec/subtype) "html"))))

(deftest weight-test
  (are [input expected]
      (= expected ((dec/weight {}) (re/input input)))
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
