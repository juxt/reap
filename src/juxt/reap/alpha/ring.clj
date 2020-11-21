;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.ring
  (:require
   [juxt.reap.alpha.decoders :as rdec]))

;; Convenience and utility functions

(defn request-preference-decoders
  "Return a map mapping Ring accept header names to their corresponding
  decoders. Only a number of known headers are checked. When content-negotiation
  algorithms require additional preferences, we recommend using this function as
  a guide to your own function."
  [request]
  (for [[header decoder]
        [["accept" rdec/accept]
         ["accept-charset" rdec/accept-charset]
         ["accept-encoding" rdec/accept-encoding]
         ["accept-language" rdec/accept-language]]
        :let [pref (get-in request [:headers header])]
        :when pref]
    [header pref decoder]))

(defn request->decoded-preferences
  "Return a map mapping Ring accept header names to their reap decoded values."
  [request]
  (into
   {}
   (for [[header pref decoder] (request-preference-decoders request)]
     [header (decoder pref)])))

(defn request->delay-decoded-preferences
  "Same as request->decoded-preferences, but each value is delayed to avoid
  unnecessary parsing. This is intended for performance sensitive
  content-negotiation algorithms."
  [request]
  (into
   {}
   (for [[header pref decoder] (request-preference-decoders request)]
     [header (delay (decoder pref))])))
