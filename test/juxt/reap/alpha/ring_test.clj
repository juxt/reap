;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.ring-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.ring :as ring]))

(deftest headers->decoded-preferences-test
  (is
   (=
    {"accept"
     [#:juxt.reap.alpha.rfc7231
      {:media-range "application/json",
       :type "application",
       :subtype "json",
       :parameters {}}]}
    (ring/headers->decoded-preferences
     {"accept" "application/json"}))))

(deftest headers->delay-decoded-preferences-test
  (let [result (ring/headers->delay-decoded-preferences
                {"accept" "application/json"})]
    (is result)
    (is (delay? (get result "accept")))
    (is (not (realized? (get result "accept"))))
    (let [val @(get result "accept")]
      (is (realized? (get result "accept")))
      (is (= [#:juxt.reap.alpha.rfc7231
              {:media-range "application/json",
               :type "application",
               :subtype "json",
               :parameters {}}] val)))))
