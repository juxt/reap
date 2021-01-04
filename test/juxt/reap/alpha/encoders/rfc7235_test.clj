;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.encoders.rfc7235-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7235 :as decoder]
   [juxt.reap.alpha.encoders.rfc7235 :as encoder]))

(defn re-encodes? [input]
  (let [auth-param-decoder (decoder/auth-param {})
        decoded (auth-param-decoder (re/input input))
        re-encoded ((encoder/auth-param {}) decoded)]
    (= input re-encoded)))

(deftest auth-param-encoding-test
  (is (re-encodes? "a=b"))
  (is (re-encodes? "a=\"b\\\" a  \"")))

(comment
  ((encoder/auth-param {})
   #:juxt.reap.alpha.rfc7235{:auth-param-name "foo" :auth-param-value "bar"}))

(deftest authorization-encoder-test
  (is
   (= "AWS4-HMAC-SHA256 Credential=\"abc/20200618/us-east-1/execute-api/aws4_request\", SignedHeaders=\"host;x-amz-date\", Signature=c6c85d0eb7b56076609570f4dbdf730d0a017208d964c615253924149ce65de5"
      ((encoder/authorization {})
       #:juxt.reap.alpha.rfc7235
       {:auth-scheme "AWS4-HMAC-SHA256",
        :auth-params
        [#:juxt.reap.alpha.rfc7235
         {:auth-param-name
          "Credential",
          :auth-param-value
          "abc/20200618/us-east-1/execute-api/aws4_request"}
         #:juxt.reap.alpha.rfc7235
         {:auth-param-name
          "SignedHeaders",
          :auth-param-value
          "host;x-amz-date"}
         #:juxt.reap.alpha.rfc7235
         {:auth-param-name
          "Signature",
          :auth-param-value
          "c6c85d0eb7b56076609570f4dbdf730d0a017208d964c615253924149ce65de5"}]}))))

(deftest challenge-encoder-test
  (is
   (=
    "Basic f8w9f98-efs789.sef8"
    ((encoder/challenge {})
     #:juxt.reap.alpha.rfc7235
     {:auth-scheme "Basic", :token68 "f8w9f98-efs789.sef8"})))
  (is
   (=
    "Newauth realm=apps, type=1, title=\"Login to \\\"apps\\\", Basic realm=\""
    ((encoder/challenge {})
     #:juxt.reap.alpha.rfc7235
     {:auth-scheme "Newauth",
      :auth-params
      [#:juxt.reap.alpha.rfc7235{:auth-param-name "realm",
                                 :auth-param-value "apps"}
       #:juxt.reap.alpha.rfc7235{:auth-param-name "type",
                                 :auth-param-value "1"}
       #:juxt.reap.alpha.rfc7235{:auth-param-name "title",
                                 :auth-param-value
                                 "Login to \"apps\", Basic realm="}]}))))

(deftest www-authenticate-encoder-test
  (is
   (=
    "Newauth realm=apps, type=1, title=\"Login to \\\"apps\\\"\", Basic realm=simple"
    ((encoder/www-authenticate {})
     [#:juxt.reap.alpha.rfc7235
      {:auth-scheme "Newauth",
       :auth-params
       [#:juxt.reap.alpha.rfc7235{:auth-param-name "realm", :auth-param-value "apps"}
        #:juxt.reap.alpha.rfc7235{:auth-param-name "type", :auth-param-value "1"}
        #:juxt.reap.alpha.rfc7235{:auth-param-name "title", :auth-param-value "Login to \"apps\""}]}
      #:juxt.reap.alpha.rfc7235
      {:auth-scheme "Basic",
       :auth-params [#:juxt.reap.alpha.rfc7235{:auth-param-name "realm", :auth-param-value "simple"}]}]))))
