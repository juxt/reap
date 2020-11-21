;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.decoders :as decoders]))

(deftest request->decoded-preferences-test
  (is
   (=
    {"accept"
     [#:juxt.reap.alpha.rfc7231
      {:media-range "application/json",
       :type "application",
       :subtype "json",
       :parameters {}}]}
    (decoders/request->decoded-preferences
     {:headers {"accept" "application/json"}}))))

(deftest request->delay-decoded-preferences-test
  (let [result (decoders/request->delay-decoded-preferences
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
