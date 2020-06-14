;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.combinators
  (:refer-clojure :exclude [comp sequence cons concat filter list constantly first second map seq apply merge array-map some contains? into])
  (:require [clojure.core :as cc]
            [clojure.string :as str]
            [juxt.reap.alpha.regex :as re])
  (:import [java.util.regex Pattern Matcher]))

(set! *warn-on-reflection* true)

;; RFC 5234 Section 3.2. Alternatives: Rule1 / Rule2
(defn alternatives [& parsers]
  (fn [matcher]
    (cc/some (fn [p] (p matcher)) parsers)))

;; RFC 5234 Section 3.3. Incremental Alternatives: Rule1 =/ Rule2

;; NOTE: This can be achieved by using alternatives recursively

;; RFC 5234 Section 3.5. Sequence Group: (Rule1 Rule2)

(defn sequence-group
  "Create a parser that matches sequentially on each of the arguments."
  [& parsers]
  (fn [matcher]
    (reduce
     (fn [acc p]
       (if-let [res (p matcher)]
         (cond-> acc (not= res :ignore) (conj res))
         (reduced nil)))
     [] parsers)))

;; RFC 5234 Section 3.6: Variable Repetition
(defn zero-or-more [parser]
  (let [super
        (fn this [matcher]
          (when-let [match (parser matcher)]
            (if (= match :ignore)
              (this matcher)
              (cc/cons match (this matcher)))))]
    (fn [matcher]
      (or (super matcher) '()))))

(defn seq [parser]
  (fn [matcher]
    (let [res (parser matcher)]
      (if (= res :ignore) res (or (cc/seq res) :ignore)))))

(defn optionally [parser]
  (fn [matcher]
    (or (parser matcher) :ignore)))

(defn comp
  "Wrap a parser in parser middleware."
  [f parser]
  (fn [matcher]
    (when-let [res (parser matcher)]
      (if (= res :ignore) res (f res)))))

(defn sequence
  "Wrap a parser in parser middleware."
  [xform parser]
  (fn [matcher]
    (cc/sequence xform (parser matcher))))

(defn cons
  [parser parsers]
  (fn [matcher]
    (if-let [fst (parser matcher)]
      (let [rst (parsers matcher)]
        (if (not= fst :ignore)
          (cc/cons fst rst)
          rst))
      '())))

(defn constantly [constant]
  (fn [_] constant))

(defn ignore [parser]
  (fn [matcher]
    (when (some? (parser matcher)) :ignore)))

(defn pattern-parser
  ([^Pattern pat]
   (pattern-parser pat {}))
  ([^Pattern pat opts]
   (fn [matcher]
     (.usePattern ^Matcher matcher pat)
     (when (.lookingAt ^Matcher matcher)
       (let [res
             (if-let [grp (:group opts)]
               (cond
                 (int? grp)
                 (.group ^Matcher matcher ^long grp)
                 (string? grp)
                 (.group ^Matcher matcher ^String grp)
                 (map? grp)
                 (reduce
                  (fn [acc [k v]]
                    (let [vl
                          (cond
                            (int? v)
                            (.group ^Matcher matcher ^long v)
                            (string? v)
                            (.group ^Matcher matcher ^String v)
                            :else
                            (throw
                             (ex-info
                              "Bad map value group type"
                              {:group v
                               :group-type (type v)})))]
                      (cond-> acc vl (conj [k vl]))))
                  {} grp)
                 :else
                 (throw
                  (ex-info
                   "Bad group type"
                   {:group grp
                    :group-type (type grp)})))
               (re-groups matcher))]
         (.region ^Matcher matcher
                  (.end ^Matcher matcher)
                  (.regionEnd ^Matcher matcher))
         res)))))

(defn first [parser]
  (fn [matcher]
    (cc/first
     (parser matcher))))

(defn second [parser]
  (fn [matcher]
    (cc/second (parser matcher))))

(defn filter [pred parser]
  (fn [matcher]
    (cc/filter pred (parser matcher))))

(defn map [f parser]
  (fn [matcher]
    (cc/map f (parser matcher))))

;; Data construction
(defn as-entry [k parser]
  (fn [matcher]
    (when-let [v (parser matcher)]
      (if (= v :ignore)
        :ignore
        [k v]))))

(defn array-map [& keyvals]
  (fn [matcher]
    (cc/reduce
     (fn [acc [k v]]
       (if-let [val (v matcher)]
         (conj acc [k val])
         (reduced nil)))
     (cc/array-map)
     (cc/partition 2 keyvals))))

(defn into [to parser]
  (fn [matcher]
    (let [res (parser matcher)]
      (when res
        (cc/into to res)))))

(defn merge [& parsers]
  (let [p (sequence-group parsers)]
    (fn [matcher]
      (cc/apply cc/merge (p matcher)))))

(defn apply [p parsers]
  (let [parser (cc/apply p parsers)]
    (fn [matcher]
      (parser matcher))))
