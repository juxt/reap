;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7231-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha :as reap]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.rfc5646 :as rfc5646]
   [juxt.reap.alpha.regex :as re]))

(deftest media-range-without-parameters-test
  (is
   (= #::rfc7231
      {:media-range "text/*"
       :type "text"
       :subtype "*"}
      ((rfc7231/media-range-without-parameters {})
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
      [#::rfc7231{:parameter-name "zip"}
       #::rfc7231{:parameter-name "qux"
                  :parameter-value "quik"}]}]
    ((::reap/decode (rfc7231/accept {}))
     (re/input "text/html ;   foo=bar ;q=0.3;zip;\t qux=quik"))))

  (testing "Bad accept headers"
    (is
     (= '()
        ((::reap/decode (rfc7231/accept {}))
         (re/input "text"))))
    (is
     (= '()
        ((::reap/decode (rfc7231/accept {}))
         (re/input "text;text/html")))))

  ;; https://www.newmediacampaigns.com/blog/browser-rest-http-accept-headers
  (testing "Firefox"
    (is
     ((::reap/decode (rfc7231/accept {}))
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"))))

  (testing "Webkit"
    (is
     ((::reap/decode (rfc7231/accept {}))
      (re/input "application/xml,application/xhtml+xml,text/html;q=0.9,\r\ntext/plain;q=0.8,image/png,*/*;q=0.5"))))

  (testing "IE"
    (is
     ((::reap/decode (rfc7231/accept {}))
      (re/input "image/jpeg, application/x-ms-application, image/gif,\r\napplication/xaml+xml, image/pjpeg, application/x-ms-xbap,\r\napplication/x-shockwave-flash, application/msword, */*"))))

  (testing "Windows 7 Chrome"
    (is
     ((::reap/decode (rfc7231/accept {}))
      (re/input "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9")))))

;; TODO: Create a very cryptic Accept test designed to catch out all but the most compliant of parsers

(deftest accept-charset-test
  (is
   (= [#::rfc7231{:charset "UTF-8", :qvalue 0.8}
       #::rfc7231{:charset "shift_JIS", :qvalue 0.4}]
      ((::reap/decode (rfc7231/accept-charset {}))
       (re/input ", \t, , , UTF-8;q=0.8,shift_JIS;q=0.4")))))

(deftest accept-encoding-test
  (is
   (= [#::rfc7231{:codings "gzip" :qvalue 0.3}
       #::rfc7231{:codings "deflate"}
       #::rfc7231{:codings "br"}]
      ((::reap/decode (rfc7231/accept-encoding {}))
       (re/input "gzip;q=0.3, deflate, br"))))
  (is (= '()
         ((::reap/decode (rfc7231/accept-encoding {}))
          (re/input "")))))

(deftest accept-language-test
  (is
   (= [#::rfc7231{:language-range "en-GB"}
       #::rfc7231{:language-range "en-US" :qvalue 0.8}
       #::rfc7231{:language-range "en" :qvalue 0.5}
       #::rfc7231{:language-range "it" :qvalue 0.3}]
      ((::reap/decode (rfc7231/accept-language {}))
       (re/input "en-GB,en-US;q=0.8,en;q=0.5,it;q=0.3"))))

  (are [input expected]
      (= expected
         ((::reap/decode (rfc7231/accept-language {})) (re/input input)))

    ", , de ;q=0.7" [#::rfc7231{:language-range "de" :qvalue 0.7}]

    "en-US ; q=1.0 ," [#::rfc7231{:language-range "en-US", :qvalue 1.0}]

    "*;q=0.9 , fr;q=0.9" [#::rfc7231{:language-range "*", :qvalue 0.9}
                          #::rfc7231{:language-range "fr", :qvalue 0.9}]

    ", *" [#::rfc7231{:language-range "*"}]

    ", , fr ;q=0.7" [#::rfc7231{:language-range "fr", :qvalue 0.7}]

    "de;q=1.0" [#::rfc7231{:language-range "de", :qvalue 1.0}]

    ", de;q=1.0" [#::rfc7231{:language-range "de", :qvalue 1.0}]

    "en-US ;q=0.7 ," [#::rfc7231{:language-range "en-US", :qvalue 0.7}]

    ", * ," [#::rfc7231{:language-range "*"}]

    ", * ,en-US ;q=0.7 , *"
    [#::rfc7231{:language-range "*"}
     #::rfc7231{:language-range "en-US", :qvalue 0.7}
     #::rfc7231{:language-range "*"}]))

(deftest allow-test
  (is
   (= [#::rfc7231{:method "GET"}
       #::rfc7231{:method "HEAD"}
       #::rfc7231{:method "PUT"}]
      ((::reap/decode (rfc7231/allow {}))
       (re/input "GET, HEAD, PUT")))))

(deftest content-encoding-test
  (is
   (= [#::rfc7231{:content-coding "gzip"}
       #::rfc7231{:content-coding "deflate"}]
      ((::reap/decode (rfc7231/content-encoding {}))
       (re/input "gzip,deflate"))))
  (is
   (= []
      ((::reap/decode (rfc7231/content-encoding {}))
       (re/input "")))))

(deftest content-language-test
  (is
   (= ["en" "de"]
      (map ::rfc5646/language ((::reap/decode (rfc7231/content-language {})) (re/input "en,de"))))
   (= [["en" "US"] ["de" nil]]
      (map (juxt ::rfc5646/language ::rfc7231/region)))))

(deftest http-date-test
  (are [input expected]
      (= expected
         ((::reap/decode (rfc7231/http-date {}))
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
  (let [decode (::reap/decode (rfc7231/imf-fixdate {}))]
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
  (let [decode (::reap/decode (rfc7231/rfc850-date {}))]
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

(deftest vary-test
  (let [vary (rfc7231/vary {})]
    (are [original expected]
        (let [decoded ((::reap/decode vary) (re/input original))
              encoded ((::reap/encode vary) decoded)]
          (= expected encoded))
      "*" "*"
      "accept" "accept"
      ",    accept" "accept"
      ",    ,, , accept" "accept"
      "accept,accept-charset" "accept, accept-charset"
      "accept, \taccept-language" "accept, accept-language")))

(deftest asctime-date-test
  (let [decode (::reap/decode (rfc7231/asctime-date {}))]
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
((::reap/decode (rfc7231/http-date {}))
 (re/input "Sun Nov  6 08:49:37 1994"))


(deftest date1-test
  (is
   (=
    #::rfc7231
    {:day "30" :month "Sep" :year "2002"}
    ((::reap/decode (rfc7231/date1 {}))
     (re/input "30 Sep 2002")))))

(deftest date2-test
  (is
   (=
    #::rfc7231
    {:day "10" :month "Apr" :year "82"}
    ((::reap/decode (rfc7231/date2 {}))
     (re/input "10-Apr-82")))))

(deftest date3-test
  (is
   (=
    #::rfc7231
    {:day " 6" :month "Nov"}
    ((::reap/decode (rfc7231/date3 {}))
     (re/input "Nov  6"))))
  (is
   (nil? ((::reap/decode (rfc7231/date3 {}))
          (re/input "Nov 6"))))
  (is
   (=
    #::rfc7231
    {:day "12" :month "Jun"}
    ((::reap/decode (rfc7231/date3 {}))
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
    ((::reap/decode
      (rfc7231/media-range
       {::reap/decode-preserve-case true}))
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
      ((::reap/decode (rfc7231/media-range {}))
       (re/input "TEXT/Html"))))))

(deftest media-type-test
  (is (= #::rfc7231{:type "text" :subtype "html" :parameters [] :parameter-map {}}
         ((::reap/decode
           (rfc7231/media-type {}))
          (re/input "text/html"))))
  (is (= #::rfc7231
         {:type "text"
          :subtype "html"
          :parameter-map {"foo" "bar" "zip" "qux"}
          :parameters [#::rfc7231{:parameter-name "foo" :parameter-value "bar"}
                       #::rfc7231{:parameter-name "ZIP" :parameter-value "qux"}]}
         ((::reap/decode
           (rfc7231/media-type
            {::reap/decode-preserve-case true}))
          (re/input "text/html;foo=bar;ZIP=qux")))))

(deftest parameter-test
  (testing "value"
    (is
     (=
      #::rfc7231{:parameter-name "foo", :parameter-value "bar"}
      ((::reap/decode (rfc7231/parameter {}))
       (re/input "foo=bar"))))
    (is
     (nil?
      ((::reap/decode (rfc7231/parameter {}))
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
      ((::reap/decode (rfc7231/parameter {}))
       (re/input "FOO=Bar"))))
    (is
     (nil?
      ((::reap/decode (rfc7231/parameter {}))
       (re/input "foo")))))

  (testing "quoted-string"
    (is
     (=
      #::rfc7231{:parameter-name "foo", :parameter-value "ba'r"}
      ((::reap/decode (rfc7231/parameter {}))
       (re/input "foo=\"ba\\'r\"")))))

  (testing "optional parameter"
    (is
     (=
      #::rfc7231{:parameter-name "foo"}
      ((::reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo"))))
    (is
     (=
      #::rfc7231{:parameter-name "foo" :parameter-value "bar"}
      ((::reap/decode (rfc7231/optional-parameter {}))
       (re/input "foo=bar"))))
    (is
     (=
      #::rfc7231{:parameter-name "foo", :parameter-value "ba'r"}
      ((::reap/decode (rfc7231/optional-parameter {}))
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
         ((::reap/decode (rfc7231/retry-after {}))
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
  (is (re-matches (re-pattern rfc7231/qvalue) "1"))
  (is (re-matches (re-pattern rfc7231/qvalue) "1.000"))
  (is (re-matches (re-pattern rfc7231/qvalue) "0.9"))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "1.001")))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "1.0000")))
  (is (nil? (re-matches (re-pattern rfc7231/qvalue) "0.1234"))))

(deftest subtype-test
  (is (= "html" (re-matches (re-pattern rfc7231/subtype) "html"))))

(deftest weight-test
  (are [input expected]
      (= expected ((::reap/decode (rfc7231/weight {})) (re/input input)))
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
