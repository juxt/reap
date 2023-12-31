;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc7234-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.regex :as re]
   [juxt.reap.rfc7234 :as rfc7234]
   [juxt.reap.decoders.rfc7234 :as dec]))

(deftest cache-control-test
  (is
   (=
    [#:juxt.reap.rfc7234{:cache-directive "foo",
                               :cache-directive-value "bar"}
     #:juxt.reap.rfc7234{:cache-directive "a"}
     #:juxt.reap.rfc7234{:cache-directive "b", :cache-directive-value "c"}
     #:juxt.reap.rfc7234{:cache-directive "d"}]

    ((dec/cache-control {}) (re/input ",,  ,, ,foo=bar,, ,  a,b=c,,,d")))))
