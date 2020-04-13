;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc4647
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.rfc5234 :as rfc5234 :refer [ALPHA DIGIT]]))

;; alphanum = ALPHA / DIGIT
(def alphanum (rfc5234/merge-alternatives ALPHA DIGIT))

;; language-range   = (1*8ALPHA *("-" 1*8alphanum)) / "*"
(defn language-range []
  (p/alternatives
   (p/pattern-parser
    (re-pattern
     (re/re-compose "[%s]{1,8}(?:\\-[%s]{1,8})*" ALPHA alphanum))
    0
    {:generator (fn [] (rand-nth ["en-US" "en-GB" "de" "fr"]))})
   (p/pattern-parser #"\*" 0 {:generator (constantly "*")})))
