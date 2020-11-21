;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7235-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7235 :as rfc7235]))

(deftest auth-param-test
  (let [p (rfc7235/auth-param {})]
    (are [input expected] (= expected (p (re/input input)))
      "a=b" #:juxt.reap.alpha.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a=\"b\"" #:juxt.reap.alpha.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a = b" #:juxt.reap.alpha.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a= b" #:juxt.reap.alpha.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a =b" #:juxt.reap.alpha.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a=\tb" #:juxt.reap.alpha.rfc7235{:auth-param-name "a" :auth-param-value "b"})))

(deftest www-authenticate-test
  (testing "Example in RFC 7235"
    (is
     (=
      [#:juxt.reap.alpha.rfc7235
       {:auth-scheme "Newauth",
        :auth-params
        [#:juxt.reap.alpha.rfc7235{:auth-param-name "realm", :auth-param-value "apps"}
         #:juxt.reap.alpha.rfc7235{:auth-param-name "type", :auth-param-value "1"}
         #:juxt.reap.alpha.rfc7235{:auth-param-name "title", :auth-param-value "Login to \"apps\""}]}
       #:juxt.reap.alpha.rfc7235
       {:auth-scheme "Basic",
        :auth-params [#:juxt.reap.alpha.rfc7235{:auth-param-name "realm", :auth-param-value "simple"}]}]

      (let [p (rfc7235/www-authenticate {})]
        (p (re/input "Newauth realm=\"apps\", type=1, \t title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"")))))))
