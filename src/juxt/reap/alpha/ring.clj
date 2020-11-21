;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.ring
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc7231 :as rfc7231]))

;; Convenience and utility functions

(def accept (rfc7231/accept {}))
(def accept-charset (rfc7231/accept-charset {}))
(def accept-encoding (rfc7231/accept-encoding {}))
(def accept-language (rfc7231/accept-language {}))

(defn request-preference-decoders
  "Return a map mapping Ring accept header names to their corresponding
  decoders. Only a number of known headers are checked. When content-negotiation
  algorithms require additional preferences, we recommend using this function as
  a guide to your own function."
  [request]
  (for [[header decoder]
        [["accept" accept]
         ["accept-charset" accept-charset]
         ["accept-encoding" accept-encoding]
         ["accept-language" accept-language]]
        :let [pref (get-in request [:headers header])]
        :when pref]
    [header pref decoder]))

(defn request->decoded-preferences
  "Return a map mapping Ring accept header names to their reap decoded values."
  [request]
  (into
   {}
   (for [[header pref decoder] (request-preference-decoders request)]
     [header (decoder (re/input pref))])))

(defn request->delay-decoded-preferences
  "Same as request->decoded-preferences, but each value is delayed to avoid
  unnecessary parsing. This is intended for performance sensitive
  content-negotiation algorithms."
  [request]
  (into
   {}
   (for [[header pref decoder] (request-preference-decoders request)]
     [header (delay (decoder (re/input pref)))])))
