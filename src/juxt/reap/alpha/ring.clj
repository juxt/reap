;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.ring
  (:require [juxt.reap.alpha.decoders :as decoders]))

(defn decode-accept-headers
  "Return a :juxt.http/request-headers entry from the headers in a Ring request."
  [request]
  [:juxt.http/request-headers
   (into
    {}
    (for [[header decoder]
          [["accept" decoders/accept]
           ["accept-charset" decoders/accept-charset]
           ["accept-encoding" decoders/accept-encoding]
           ["accept-language" decoders/accept-language]]
          :let [hv (get-in request [:headers header])]
          :when hv]
      [header (decoder hv)]))])
