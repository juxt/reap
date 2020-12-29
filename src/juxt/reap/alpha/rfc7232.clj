;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7232)

(defn strong-compare-match?
  [etag-1 etag-2]
  (assert etag-1)
  (assert etag-2)
  (and
   (not (:juxt.reap.alpha.rfc7232/weak? etag-1))
   (not (:juxt.reap.alpha.rfc7232/weak? etag-2))
   (= (:juxt.reap.alpha.rfc7232/opaque-tag etag-1)
      (:juxt.reap.alpha.rfc7232/opaque-tag etag-2))))

(defn weak-compare-match?
  [etag-1 etag-2]
  (assert etag-1)
  (assert etag-2)
  (= (:juxt.reap.alpha.rfc7232/opaque-tag etag-1)
     (:juxt.reap.alpha.rfc7232/opaque-tag etag-2)))
