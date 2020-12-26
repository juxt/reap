;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7232-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.alpha.decoders.rfc7232 :refer [entity-tag]]
   [juxt.reap.alpha.rfc7232 :refer [strong-compare-match? weak-compare-match?]]
   [juxt.reap.alpha.regex :as re]))

(defn parse-entity-tag [s]
  ((entity-tag {}) (re/input s)))

(deftest etag-comparison-test
  (testing "Strong comparison"
    (is
     (not
      (strong-compare-match?
       (parse-entity-tag "W/\"1\"")
       (parse-entity-tag "W/\"1\""))))
    (is
     (not
      (strong-compare-match?
       (parse-entity-tag "W/\"1\"")
       (parse-entity-tag "W/\"2\""))))
    (is
     (not
      (strong-compare-match?
       (parse-entity-tag "W/\"1\"")
       (parse-entity-tag "\"1\""))))
    (is
     (strong-compare-match?
      (parse-entity-tag "\"1\"")
      (parse-entity-tag "\"1\""))))

  (testing "Weak comparison"
    (is
     (weak-compare-match?
      (parse-entity-tag "W/\"1\"")
      (parse-entity-tag "W/\"1\"")))
    (is
     (not
      (weak-compare-match?
       (parse-entity-tag "W/\"1\"")
       (parse-entity-tag "W/\"2\""))))
    (is
     (weak-compare-match?
      (parse-entity-tag "W/\"1\"")
      (parse-entity-tag "\"1\"")))
    (is
     (weak-compare-match?
      (parse-entity-tag "\"1\"")
      (parse-entity-tag "\"1\"")))))
