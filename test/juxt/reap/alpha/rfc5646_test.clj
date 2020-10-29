;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc5646-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.alpha :as reap]
   [juxt.reap.alpha.rfc5646 :as rfc5646]
   [juxt.reap.alpha.regex :as re]))

;; From https://tools.ietf.org/html/rfc5646#appendix-A

(deftest language-tag-test

  (let [p (::reap/decode (rfc5646/language-tag {}))]

    (is (= #::rfc5646{:lang-type :langtag
                      :langtag "de"
                      :language "de"
                      :code "de"}
           (p (re/input "de"))))

    (is (= #::rfc5646{:lang-type :langtag
                      :langtag "fr"
                      :language "fr"
                      :code "fr"}
           (p (re/input "fr"))))

    (is (= #::rfc5646{:lang-type :langtag
                      :langtag "ja"
                      :language "ja"
                      :code "ja"}
           (p (re/input "ja"))))

    (testing "Simple language subtag"
      (is (= #::rfc5646{:langtag "i-enochian"
                        :lang-type :grandfathered
                        :lang-subtype :irregular}
             (p (re/input "i-enochian")))))

    (testing "Language subtag plus script subtag"

      ;; Chinese written using the Traditional Chinese script
      (is (= #::rfc5646{:lang-type :langtag
                        :langtag "zh-Hant"
                        :language "zh"
                        :code "zh"
                        :script "Hant"}
             (p (re/input "zh-Hant"))))

      ;; Chinese written using the Simplified Chinese script
      (is (= #::rfc5646{:lang-type :langtag
                        :langtag "zh-Hans"
                        :language "zh"
                        :code "zh"
                        :script "Hans"}
             (p (re/input "zh-Hans"))))

      ;; Serbian written using the Cyrillic script
      (is (= #::rfc5646{:lang-type :langtag
                        :langtag "sr-Cyrl"
                        :language "sr"
                        :code "sr"
                        :script "Cyrl"}
             (p (re/input "sr-Cyrl"))))

      ;; Serbian written using the Latin script
      (is (= #::rfc5646
             {:lang-type :langtag
              :langtag "sr-Latn"
              :language "sr"
              :code "sr"
              :script "Latn"}
             (p (re/input "sr-Latn")))))

    (testing "Extended language subtags and their primary language subtag counterparts"

      ;; Chinese, Mandarin, Simplified script, as used in China
      (is (= #::rfc5646
             {:lang-type :langtag
              :langtag "zh-cmn-Hans-CN"
              :language "zh-cmn"
              :code "zh"
              :extlang "cmn"
              :script "Hans"
              :region "CN"}
             (p (re/input "zh-cmn-Hans-CN"))))

      ;; Mandarin Chinese, Simplified script, as used in China
      (is (= #::rfc5646
             {:lang-type :langtag
              :langtag "cmn-Hans-CN"
              :language "cmn"
              :code "cmn"
              :script "Hans"
              :region "CN"}
             (p (re/input "cmn-Hans-CN"))))

      ;; Chinese, Cantonese, as used in Hong Kong SAR
      (is (= #::rfc5646
             {:lang-type :langtag
              :langtag "zh-yue-HK"
              :language "zh-yue"
              :code "zh"
              :extlang "yue"
              :region "HK"}
             (p (re/input "zh-yue-HK"))))

      ;; Cantonese Chinese, as used in Hong Kong SAR
      (is (= #::rfc5646
             {:lang-type :langtag
              :langtag "yue-HK"
              :language "yue"
              :code "yue"
              :region "HK"}
             (p (re/input "yue-HK"))))

      (testing "Language-Script-Region"

        ;; Chinese written using the Simplified script as used in mainland China
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "zh-Hans-CN"
                :language "zh"
                :code "zh"
                :script "Hans"
                :region "CN"}
               (p (re/input "zh-Hans-CN"))))

        ;; Serbian written using the Latin script as used in Serbia
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "sr-Latn-RS"
                :language "sr"
                :code "sr"
                :script "Latn"
                :region "RS"}
               (p (re/input "sr-Latn-RS")))))

      (testing "Language-Variant"
        ;; Resian dialect of Slovenian
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "sl-rozaj"
                :language "sl"
                :code "sl"
                :variant "rozaj"}
               (p (re/input "sl-rozaj"))))

        ;; San Giorgio dialect of Resian dialect of Slovenian
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "sl-rozaj-biske"
                :language "sl"
                :code "sl"
                :variant "biske"}
               (p (re/input "sl-rozaj-biske"))))

        ;; Nadiza dialect of Slovenian
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "sl-nedis"
                :language "sl"
                :code "sl"
                :variant "nedis"}
               (p (re/input "sl-nedis")))))

      (testing "Language-Region-Variant"

        ;; German as used in Switzerland using the 1901 variant [orthography]
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "de-CH-1901"
                :language "de"
                :code "de"
                :region "CH"
                :variant "1901"}
               (p (re/input "de-CH-1901"))))

        ;; Slovenian as used in Italy, Nadiza dialect
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "sl-IT-nedis"
                :language "sl"
                :code "sl"
                :region "IT"
                :variant "nedis"}
               (p (re/input "sl-IT-nedis")))))

      (testing "Language-Script-Region-Variant"
        ;; Eastern Armenian written in Latin script, as used in Italy
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "hy-Latn-IT-arevela"
                :language "hy"
                :code "hy"
                :script "Latn"
                :region "IT"
                :variant "arevela"}
               (p (re/input "hy-Latn-IT-arevela")))))

      (testing "Language-Region"
        ;; German for Germany
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "de-DE"
                :language "de"
                :code "de"
                :region "DE"}
               (p (re/input "de-DE"))))

        ;; English as used in the United States
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "en-US"
                :language "en"
                :code "en"
                :region "US"}
               (p (re/input "en-US"))))

        ;; Spanish appropriate for the Latin America and Caribbean region using
        ;; the UN region code
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "es-419"
                :language "es"
                :code "es"
                :region "419"}
               (p (re/input "es-419")))))

      (testing "Private use subtags"

        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "de-CH-x-phonebk"
                :language "de"
                :code "de"
                :region "CH"
                :privateuse "x-phonebk"
                :privatetag "phonebk"}
               (p (re/input "de-CH-x-phonebk"))))

        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "az-Arab-x-AZE-derbend"
                :language "az"
                :code "az"
                :script "Arab",
                :privateuse "x-AZE-derbend"
                :privatetag "AZE-derbend"}
               (p (re/input "az-Arab-x-AZE-derbend")))))

      (testing "Private use registry values"

        ;; Private use using the singleton 'x'
        (is (= #::rfc5646
               {:lang-type :privateuse
                :langtag "x-whatever"
                :privatetag "whatever"}
               (p (re/input "x-whatever"))))

        ;; All private tags
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "qaa-Qaaa-QM-x-southern"
                :language "qaa"
                :code "qaa"
                :script "Qaaa"
                :region "QM"
                :privateuse "x-southern"
                :privatetag "southern"}
               (p (re/input "qaa-Qaaa-QM-x-southern"))))

        ;; German, with a private script
        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "de-Qaaa"
                :language "de"
                :code "de"
                :script "Qaaa"}
               (p (re/input "de-Qaaa"))))

        ;; Serbian, Latin script, private region
        (is (= #::rfc5646
               {:lang-type :langtag,
                :langtag "sr-Latn-QM"
                :language "sr"
                :code "sr"
                :script "Latn"
                :region "QM"}
               (p (re/input "sr-Latn-QM"))))

        ;; Serbian, private script, for Serbia
        (is (= #::rfc5646
               {:lang-type :langtag,
                :langtag "sr-Qaaa-RS"
                :language "sr"
                :code "sr"
                :script "Qaaa"
                :region "RS"}
               (p (re/input "sr-Qaaa-RS")))))

      (testing "Tags that use extensions"

        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "en-US-u-islamcal"
                :language "en"
                :code "en"
                :region "US"
                :extension "u-islamcal"}
               (p (re/input "en-US-u-islamcal"))))

        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "zh-CN-a-myext-x-private"
                :language "zh"
                :code "zh"
                :region "CN"
                :extension "a-myext"
                :privateuse "x-private"
                :privatetag "private"}
               (p (re/input "zh-CN-a-myext-x-private"))))

        (is (= #::rfc5646
               {:lang-type :langtag
                :langtag "en-a-myext-b-another"
                :language "en"
                :code "en"
                :extension "b-another"}
               (p (re/input "en-a-myext-b-another")))))

      (testing "Some Invalid Tags"
        ;; Two region tags
        (is (nil? (p (re/input "de-419-DE"))))

        ;; Use of a single-character subtag in primary position; note that there
        ;; are a few grandfathered tags that start with "i-" that are valid
        (is (nil? (p (re/input "a-DE"))))

        ;; Two extensions with same single-letter prefix
        ;; TODO: Fix failing test
        (comment
          (is (nil? (p (re/input "ar-a-aaa-b-bbb-a-ccc")))))))

    (testing "comma at end of input"
      (is (= #::rfc5646
             {:lang-type :langtag
              :langtag "en-US"
              :language "en"
              :code "en"
              :region "US"}
             (p (re/input "en-US,")))))))
