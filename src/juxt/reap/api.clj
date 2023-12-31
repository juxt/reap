;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.api
  (:require
   [juxt.reap.regex :as re])
  (:import [java.util.regex Matcher]))

;; Entry point with error handling

(defn decode [parser s]
  (try
    (parser (re/input s))
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [^Matcher matcher message] :as ex-data} (ex-data e)]
        (throw
         (ex-info
          (format
           "Parsing failed at position %d. %s Input was '%s'"
           (.regionStart matcher)
           message
           s)
          (conj
           ex-data [:input s])))))))
