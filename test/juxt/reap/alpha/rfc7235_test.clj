;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc5234-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7235 :as rfc7235]))

(deftest auth-param-test
  (let [p (rfc7235/auth-param)]
    (are [input expected] (= expected (p (re/input input)))
      "a=b" {:name "a" :value "b"}
      "a=\"b\"" {:name "a" :value "b"}
      "a = b" {:name "a" :value "b"}
      "a= b" {:name "a" :value "b"}
      "a =b" {:name "a" :value "b"}
      "a=\tb" {:name "a" :value "b"})))

(deftest www-authenticate-test
  (testing "Example in RFC 7235"
    (is
     (=
      [{:auth-scheme "Newauth",
        :auth-params
        [{:name "realm", :value "apps"}
         {:name "type", :value "1"}
         {:name "title", :value "Login to \"apps\""}]}
       {:auth-scheme "Basic",
        :auth-params [{:name "realm", :value "simple"}]}]

      (let [p (rfc7235/www-authenticate)]
        (p (re/input "Newauth realm=\"apps\", type=1, \t title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"")))))))
