;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.encoders.rfc6570
  (:require
   [juxt.reap.interval :as interval]
   [juxt.reap.decoders.rfc3986 :as rfc3986]
   [juxt.reap.decoders.rfc5234 :as rfc5234]))

(def unreserved
  (set
   (interval/unroll rfc3986/unreserved)))

(defn pct-encode [s]
  (assert (string? s))
  (apply str
         (for [c s]
           (if (contains? unreserved c)
             c
             (format "%%%X" (int c))))))

(def reserved
  (set
   (interval/unroll
    (rfc5234/merge-alternatives
     rfc3986/unreserved
     rfc3986/reserved))))

(defn pct-encode-reserved [s]
  (assert (string? s))
  (apply str
         (for [c s]
           (if (contains? reserved c)
             c
             (format "%%%x" (int c))))))
