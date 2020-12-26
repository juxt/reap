;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.encoders
  (:require
   [juxt.reap.alpha.encoders.rfc7231 :as rfc7231]))

(def ^:private precompiled-http-date (rfc7231/http-date {}))

(defn format-http-date [^java.util.Date inst]
  (assert (instance? java.util.Date inst) (format "Type is %s" (type inst)))
  (precompiled-http-date {:juxt.reap.alpha.rfc7231/date inst}))
