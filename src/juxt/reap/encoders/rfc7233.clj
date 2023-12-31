;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.encoders.rfc7233
  (:require
   [juxt.reap.rfc7233 :as rfc7233]))

;; Content-Range = byte-content-range / other-content-range

;; HTTP-date = IMF-fixdate / obs-date
(defn content-range [_]
  (fn [{::rfc7233/keys [units first-byte-pos last-byte-pos complete-length range-resp] :as decoded}]
    (cond
      (= units "bytes")
      (cond
        (and first-byte-pos last-byte-pos complete-length)
        (format "%s %s-%s/%s" units first-byte-pos last-byte-pos complete-length)
        (and (nil? first-byte-pos) (nil? last-byte-pos) complete-length)
        (format "%s */%s" units complete-length)
        :else (throw (ex-info "Malformed argument" {:arg decoded})))

      :else
      (cond
        (and units range-resp)
        (format "%s %s" units range-resp)
        :else (throw (ex-info "Malformed argument" {:arg decoded}))))))
