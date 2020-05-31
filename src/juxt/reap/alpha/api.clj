;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.api
  (:require
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.regex :as re]))

(def ^:private precompiled-accept (rfc7231/accept))

(defn accept [s]
  (precompiled-accept (re/input s)))
