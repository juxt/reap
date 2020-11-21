;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7235-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.alpha :as reap]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7235 :as rfc7235]))

(deftest auth-param-test
  (let [p (::reap/decode (rfc7235/auth-param {}))]
    (are [input expected] (= expected (p (re/input input)))
      "a=b" #::rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a=\"b\"" #::rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a = b" #::rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a= b" #::rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a =b" #::rfc7235{:auth-param-name "a" :auth-param-value "b"}
      "a=\tb" #::rfc7235{:auth-param-name "a" :auth-param-value "b"})))

(deftest www-authenticate-test
  (testing "Example in RFC 7235"
    (is
     (=
      [#::rfc7235
       {:auth-scheme "Newauth",
        :auth-params
        [#::rfc7235{:auth-param-name "realm", :auth-param-value "apps"}
         #::rfc7235{:auth-param-name "type", :auth-param-value "1"}
         #::rfc7235{:auth-param-name "title", :auth-param-value "Login to \"apps\""}]}
       #::rfc7235
       {:auth-scheme "Basic",
        :auth-params [#::rfc7235{:auth-param-name "realm", :auth-param-value "simple"}]}]

      (let [p (::reap/decode (rfc7235/www-authenticate {}))]
        (p (re/input "Newauth realm=\"apps\", type=1, \t title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"")))))))

(defn re-encodes? [input]
  (let [auth-param (rfc7235/auth-param nil)
        decoded ((::reap/decode auth-param) (re/input input))
        re-encoded ((::reap/encode auth-param) decoded)]
    (= input re-encoded)))

(deftest auth-param-encoding-test
  (is (re-encodes? "a=b"))
  (is (re-encodes? "a=\"b\\\" a  \"")))

(comment
  ((::reap/encode (rfc7235/auth-param nil))
   #::rfc7235{:auth-param-name "foo" :auth-param-value "bar"}))

(deftest authorization-encoding-test
  (is
   (= "AWS4-HMAC-SHA256 Credential=\"abc/20200618/us-east-1/execute-api/aws4_request\", SignedHeaders=\"host;x-amz-date\", Signature=c6c85d0eb7b56076609570f4dbdf730d0a017208d964c615253924149ce65de5"
      ((::reap/encode (rfc7235/authorization {}))
       #::rfc7235{:auth-scheme "AWS4-HMAC-SHA256",
                  :auth-params
                  [#::rfc7235{:auth-param-name
                              "Credential",
                              :auth-param-value
                              "abc/20200618/us-east-1/execute-api/aws4_request"}
                   #::rfc7235{:auth-param-name
                              "SignedHeaders",
                              :auth-param-value
                              "host;x-amz-date"}
                   #::rfc7235{:auth-param-name
                              "Signature",
                              :auth-param-value
                              "c6c85d0eb7b56076609570f4dbdf730d0a017208d964c615253924149ce65de5"}]}))))
