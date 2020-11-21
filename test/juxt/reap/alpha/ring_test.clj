;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.ring-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.ring :as ring]))

(deftest request->decoded-preferences-test
  (is
   (=
    {"accept"
     [#:juxt.reap.alpha.rfc7231
      {:media-range "application/json",
       :type "application",
       :subtype "json",
       :parameters {}}]}
    (ring/request->decoded-preferences
     {:headers {"accept" "application/json"}}))))

(deftest request->delay-decoded-preferences-test
  (let [result (ring/request->delay-decoded-preferences
                {:headers {"accept" "application/json"}})]
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
