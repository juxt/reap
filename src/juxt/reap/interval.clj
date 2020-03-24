;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.interval
  (:refer-clojure :exclude [contains?])
  (:require
   [clojure.pprint :as pprint]
   [juxt.reap.regex :as re]))

;; Intervals

(defprotocol IInterval
  (beginning [_] "The beginning of the range, as an int (inclusive)")
  (end [_] "The end of the range, as an int (inclusive)"))

(defrecord Interval [beginning end]
  IInterval
  (beginning [_] beginning)
  (end [_] end))

(extend-protocol re/RegularExpressionPattern
  Interval
  (re-str [ival] (format "[%s-%s]"
                         (re/int->regex (beginning ival))
                         (re/int->regex (end ival)))))

(defmethod print-method Interval [obj writer]
  (if *print-readably*
    (.write writer (format "#juxt.reap/interval [%s %s]" (beginning obj) (end obj)))
    (.write writer (format "%%x%02X-%%x%02X" (beginning obj) (end obj)))))

(defmethod pprint/simple-dispatch Interval [obj]
  (.write *out* (format "[%%x%02X-%%x%02X]" (beginning obj) (end obj))))

(extend-protocol IInterval
  java.lang.Integer
  (beginning [n] n)
  (end [n] n)
  java.lang.Character
  (beginning [c] (beginning (int c)))
  (end [c] (end (int c)))
  java.lang.Long
  (beginning [n] n)
  (end [n] n))

(defprotocol IIntervalCoercions
  (->interval [_] "Coerce to an interval"))

(extend-protocol IIntervalCoercions
  clojure.lang.APersistentVector
  (->interval [v]
    (if (= (count v) 2)
      (->Interval (int (first v)) (int (second v)))
      (throw (ex-info "Vector must have two items")))))

;; Allen's Basic Relations

(defn precedes? [x y]
  (< (inc (end x)) (beginning y)))

(defn equals? [x y]
  (and
    (= (beginning x) (beginning y))
    (= (end x) (end y))))

(defn meets? [x y]
  (= (inc (end x)) (beginning y)))

(defn overlaps? [x y]
  (and
   (< (beginning x) (beginning y))
   (> (inc (end x)) (beginning y))
   (< (end x) (end y))))

(defn during? [x y]
  (and
   (> (beginning x) (beginning y))
   (< (end x) (end y))))

(defn starts? [x y]
  (and
   (= (beginning x) (beginning y))
   (< (end x) (end y))))

(defn finishes? [x y]
  (and
   (> (beginning x) (beginning y))
   (= (end x) (end y))))

;; Six pairs of the relations are converses.  For example, the
;; converse of "a precedes b" is "b preceded by a"; whenever the first
;; relation is true, its converse is true also.

(defn conv
  "The converse of a basic relation."
  [f]
  (fn [x y]
    (f y x)))

(defn preceded-by? [x y] ((conv precedes?) x y))
(defn met-by? [x y] ((conv meets?) x y))
(defn overlapped-by? [x y] ((conv overlaps?) x y))
(defn finished-by? [x y] ((conv finishes?) x y))

;; contains? is semantically similar to tick.core/coincident?
(defn contains? [x y] ((conv during?) x y))
(defn started-by? [x y] ((conv starts?) x y))

(def relation-name
  {precedes? :precedes
   meets? :meets
   starts? :starts
   during? :during
   finishes? :finishes
   overlaps? :overlaps
   equals? :equals
   contains? :contains
   started-by? :started-by
   finished-by? :finished-by
   overlapped-by? :overlapped-by
   met-by? :met-by
   preceded-by? :preceded-by})

(def basic-relations
  [precedes? meets? overlaps? finished-by? contains?
   starts? equals? started-by? during? finishes? overlapped-by?
   met-by? preceded-by?])

(defn some-of [& relations]
  (fn [x y]
    (some (fn [f] (when (f x y) f)) relations)))

(def basic-relation (apply some-of basic-relations))

(defn relation [x y]
  (relation-name (basic-relation x y)))

(def disjoint? (some-of precedes? preceded-by? meets? met-by?))

(def apart? (some-of precedes? preceded-by?))

(def spliceable? (some-of meets? starts? overlaps? equals? contains? started-by? finished-by?))

(defn splice
  "Combine two chars or ranges into a range. Assumes x and y are
  spliceable, which means that the beginning of x is prior or equal to
  the beginning of y, and that the two values can be spliced (they are
  touching and not apart)."
  [x y]
  (if (spliceable? x y)
    (->interval
     [(beginning x)
      (if (> (end x) (end y)) (end x) (end y))])
    (throw (ex-info "Unspliceable values/ranges" {:error ::unspliceable :x x :y y}))))
