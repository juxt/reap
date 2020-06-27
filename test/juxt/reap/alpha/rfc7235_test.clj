;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc5234-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7235 :as rfc7235]))

(deftest auth-param-test
  (let [p (rfc7235/auth-param)]
    (are [input expected] (= expected (p (re/input input)))
      "a=b" #:juxt.reap.alpha{:parameter-name "a" :parameter-value "b"}
      "a=\"b\"" #:juxt.reap.alpha{:parameter-name "a" :parameter-value "b"}
      "a = b" #:juxt.reap.alpha{:parameter-name "a" :parameter-value "b"}
      "a= b" #:juxt.reap.alpha{:parameter-name "a" :parameter-value "b"}
      "a =b" #:juxt.reap.alpha{:parameter-name "a" :parameter-value "b"}
      "a=\tb" #:juxt.reap.alpha{:parameter-name "a" :parameter-value "b"})))

(deftest www-authenticate-test
  (testing "Example in RFC 7235"
    (is
     (=
      [#:juxt.reap.alpha
       {:auth-scheme "Newauth",
        :auth-params
        [#:juxt.reap.alpha{:parameter-name "realm", :parameter-value "apps"}
         #:juxt.reap.alpha{:parameter-name "type", :parameter-value "1"}
         #:juxt.reap.alpha{:parameter-name "title", :parameter-value "Login to \"apps\""}]}
       #:juxt.reap.alpha
       {:auth-scheme "Basic",
        :auth-params [#:juxt.reap.alpha{:parameter-name "realm", :parameter-value "simple"}]}]

      (let [p (rfc7235/www-authenticate)]
        (p (re/input "Newauth realm=\"apps\", type=1, \t title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"")))))))
