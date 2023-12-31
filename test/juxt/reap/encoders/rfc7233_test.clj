;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.encoders.rfc7233-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.encoders.rfc7233 :as enc]
   [juxt.reap.rfc7233 :as rfc7233]))

(deftest content-range-test
  (is
   (=
    "bytes */1000"
    ((enc/content-range {}) #::rfc7233{:units "bytes" :complete-length 1000})))

  (is
   (=
    "bytes 10-20/1000"
    ((enc/content-range {}) #::rfc7233{:units "bytes"
                                       :first-byte-pos 10
                                       :last-byte-pos 20
                                       :complete-length 1000})))

  (is
   (=
    "pages 1-2"
    ((enc/content-range {}) #::rfc7233{:units "pages"
                                       :range-resp "1-2"}))))
