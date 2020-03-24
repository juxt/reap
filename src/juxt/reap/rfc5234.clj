;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.rfc5234
  (:require
   [juxt.reap.interval :as i]))

(defn sort-by-beginning
  "Some functions require the collection to be ordered. Rather than sort
  already sorted collections, this function is available to be called
  where prior sorting is necessary."
  [coll]
  (sort-by i/beginning coll))

(defn normalize
  "Normalize the given ordered collection. A collection is normal if it
  is ordered, and each member is disjoint."
  [coll]
  (lazy-seq
   (let [[x y & coll] coll]
     (if y
       (if (i/precedes? x y)
         (cons x (normalize (cons y coll)))
         ;; Ordering is implicitly asserted by splice asserting spiceable
         (try
           (normalize (cons (i/splice x y) coll))
           (catch clojure.lang.ExceptionInfo e
             (if (= ::unspliceable (:error (ex-data e)))
               (throw
                (ex-info
                 "Input to normalize is detected to be unordered"
                 {:error ::unsupported-unordered-input} e))
               (throw e)))))
       (if x (list x) '())))))

(defn alternatives
  "Create an ordered collection of values and intervals, as
  defined by RFC 5234 Section 3.2"
  [& vs-or-ivals]
  (normalize (sort-by-beginning vs-or-ivals)))

;; There's a few caveats with the implementation of this function.
;; The function can return an unordered collection if given unordered
;; collections in its input, without spotting the error.
;; For example, if (first c1) after (second c1), the error won't be spotted.

(defn- merge-alternatives* [& colls]
  (lazy-seq
   (let [[c1 c2 & r]
         (->> colls
              (remove nil?)
              (sort-by #(i/beginning (first %))))]
     (if (nil? c2)
       c1
       (if (i/apart? (first c1) (first c2))
         (cons (first c1) (apply merge-alternatives* (apply list (next c1) c2 r)))
         (apply merge-alternatives* (apply list
                                           (cons (i/splice (first c1) (first c2))
                                                 (next c1))
                                           (next c2)
                                           r)))))))

(defprotocol AlternativesCoercion
  (as-alternatives [_] "Coerce to alternatives"))

(extend-protocol AlternativesCoercion
  clojure.lang.ISeq
  (as-alternatives [coll] coll)
  juxt.reap.interval.Interval
  (as-alternatives [ival] [ival])
  clojure.lang.PersistentHashSet
  (as-alternatives [s] (apply alternatives s)))

(defn merge-alternatives [& colls]
  (apply merge-alternatives* (map as-alternatives colls)))

;; Section B.1

(def ALPHA (alternatives
            #juxt.reap/interval [\A \Z]
            #juxt.reap/interval [\a \z]))

(def BIT (alternatives \0 \1))

(def CHAR #juxt.reap/interval [0x01 0x7F])

(def CR \return)

(def CRLF (str \return \newline))

(def DIGIT #juxt.reap/interval [\0 \9])

(def DQUOTE \")

(def HEXDIG (alternatives DIGIT \A \B \C \D \E \F))

(def HTAB \tab)

(def LF \newline)

;;(def LWSP)

(def OCTET #juxt.reap/interval [0x00 0xFF])

(def SP \space)

(def VCHAR #juxt.reap/interval [0x21 0x7E])

(def WSP (alternatives SP HTAB))
