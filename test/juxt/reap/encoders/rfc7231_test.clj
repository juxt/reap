;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.encoders.rfc7231-test
  (:require
   [clojure.test :refer [deftest are]]
   [juxt.reap.encoders.rfc7231 :as enc]
   [juxt.reap.decoders.rfc7231 :as dec]
   [juxt.reap.regex :as re]))

(deftest vary-test
  (are [original expected]
      (let [decoded ((dec/vary {}) (re/input original))
            encoded ((enc/vary {}) decoded)]
        (= expected encoded))
    "*" "*"
    "accept" "accept"
    ",    accept" "accept"
    ",    ,, , accept" "accept"
    "accept,accept-charset" "accept, accept-charset"
    "accept, \taccept-language" "accept, accept-language"))
