;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc7235-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.regex :as re]
   [juxt.reap.decoders.rfc7235 :as rfc7235]))

(deftest auth-param-test
  (let [p (rfc7235/auth-param {})]
    (are [input expected] (= expected (p (re/input input)))
      "a=b" #:juxt.reap.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a=\"b\"" #:juxt.reap.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a = b" #:juxt.reap.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a= b" #:juxt.reap.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a =b" #:juxt.reap.rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a=\tb" #:juxt.reap.rfc7235{:auth-param-name "a" :auth-param-value "b"})))

(deftest www-authenticate-test
  (testing "Example in RFC 7235"
    (is
     (=
      [#:juxt.reap.rfc7235
       {:auth-scheme "Newauth",
        :auth-params
        [#:juxt.reap.rfc7235{:auth-param-name "realm", :auth-param-value "apps"}
         #:juxt.reap.rfc7235{:auth-param-name "type", :auth-param-value "1"}
         #:juxt.reap.rfc7235{:auth-param-name "title", :auth-param-value "Login to \"apps\""}]}
       #:juxt.reap.rfc7235
       {:auth-scheme "Basic",
        :auth-params [#:juxt.reap.rfc7235{:auth-param-name "realm", :auth-param-value "simple"}]}]

      (let [p (rfc7235/www-authenticate {})]
        (p (re/input "Newauth realm=\"apps\", type=1, \t title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"")))))))


(deftest challenge-test
  (is
   (=
    #:juxt.reap.rfc7235{:auth-scheme "Basic", :token68 "f8w9f98-efs789.sef8"}
    ((rfc7235/challenge {})
     (re/input "Basic f8w9f98-efs789.sef8"))))
  (is
   (=
    #:juxt.reap.rfc7235
    {:auth-scheme "Newauth",
     :auth-params
     [#:juxt.reap.rfc7235{:auth-param-name "realm"
                                :auth-param-value "apps"}
      #:juxt.reap.rfc7235{:auth-param-name "type"
                                :auth-param-value "1"}
      #:juxt.reap.rfc7235{:auth-param-name "title"
                                :auth-param-value
                                "Login to \"apps\", Basic realm="}]}
    ((rfc7235/challenge {})
     (re/input "Newauth realm=\"apps\", type=1, title=\"Login to \\\"apps\\\", Basic realm=\"simple\"")))))

(deftest authorization-test
  (is
   (=
    #:juxt.reap.rfc7235{:auth-scheme "Basic", :token68 "YTpi"}
    ((rfc7235/authorization {}) (re/input "Basic YTpi")))))
