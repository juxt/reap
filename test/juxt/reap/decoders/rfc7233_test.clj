;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc7233-test
  (:require
   [clojure.test :refer [deftest is are]]
   [juxt.reap.regex :as re]
   [juxt.reap.rfc7231 :as rfc7231]
   [juxt.reap.rfc7232 :as rfc7232]
   [juxt.reap.rfc7233 :as rfc7233]
   [juxt.reap.decoders.rfc7233 :as dec]))

(deftest acceptable-ranges-test
  (is
   (=
    ["foo" "zip" "bar"]
    ((dec/acceptable-ranges {})
     (re/input "foo,zip,  bar")))))

;;((dec/content-range {}) (re/input "pages 20-30/1000"))

(deftest content-range-test
  (let [decode (dec/content-range {})]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "bytes 20-30/1000"
        #::rfc7233{:units "bytes"
                   :first-byte-pos 20
                   :last-byte-pos 30
                   :complete-length 1000}

        "bytes */100"
        #::rfc7233{:units "bytes"
                   :complete-length 100})))

(deftest range-test
  (let [decode (dec/range {})]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "bytes=10-20"
        #::rfc7233{:units "bytes"
                   :byte-range-set
                   [#::rfc7233{:first-byte-pos 10
                               :last-byte-pos 20}]}

        "bytes=10-40  ,  70-,-100"
        #::rfc7233{:units "bytes"
                   :byte-range-set
                   [#::rfc7233{:first-byte-pos 10
                               :last-byte-pos 40}
                    #::rfc7233{:first-byte-pos 70}
                    #::rfc7233{:suffix-length 100}]}

        "pages=abc"
        #::rfc7233{:units "pages"
                   :range-set "abc"})

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"\QExtraneous input\E"
         (decode (re/input "bytes=-10-"))))))

(deftest if-range-test
  (let [decode (dec/if-range {})]
    (are [input expected]
        (= expected
           (decode (re/input input)))

        "W/\"foo\""
        #::rfc7232{:entity-tag
                   #::rfc7232{:weak? true
                              :opaque-tag "\"foo\""}}

        "Tue, 29 Dec 2020 00:41:56 GMT"
        #::rfc7231{:http-date
                   #::rfc7231{:day "29"
                              :date
                              #inst "2020-12-29T00:41:56.000-00:00"
                              :hour "00"
                              :second "56"
                              :imf-fixdate
                              "Tue, 29 Dec 2020 00:41:56 GMT"
                              :month "Dec"
                              :day-name "Tue"
                              :year "2020"
                              :minute "41"}})))
