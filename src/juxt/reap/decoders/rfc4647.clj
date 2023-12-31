;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc4647
  (:require
   [juxt.reap.regex :as re]
   [juxt.reap.combinators :as p]
   [juxt.reap.decoders.rfc5234 :as rfc5234 :refer [ALPHA DIGIT]]))

;; alphanum = ALPHA / DIGIT
(def alphanum (rfc5234/merge-alternatives ALPHA DIGIT))

;; language-range   = (1*8ALPHA *("-" 1*8alphanum)) / "*"
(defn language-range [_]
  (p/alternatives
   (p/pattern-parser
    (re-pattern
     (re/re-compose "[%s]{1,8}(?:\\-[%s]{1,8})*" ALPHA alphanum)))
   (p/pattern-parser #"\*")))
