;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.parse
  (:refer-clojure :exclude [comp sequence cons concat]))

(set! *warn-on-reflection* true)

(defn zero-or-more [parser]
  (let [super
        (fn this [matcher]
          (when-let [match (parser matcher)]
            (lazy-seq
             (clojure.core/cons match (this matcher)))))]
    (fn [matcher]
      (super matcher))))

(defn optionally [parser]
  (fn [matcher]
    (or (parser matcher) true)))

(defn concat
  "Create a parser that matches sequentially on each of the arguments."
  [& parsers]
  (fn [matcher]
    (let [f (fn f [[p & ps]]
              (when p
                (lazy-seq
                 (when-let [x (p matcher)]
                   (clojure.core/cons x (f ps))))))
          res (f parsers)]
      (when (seq res) (vec res)))))

(defn comp
  "Wrap a parser in parser middleware."
  [f parser]
  (fn [matcher]
    (f (parser matcher))))

(defn sequence
  "Wrap a parser in parser middleware."
  [xform parser]
  (fn [matcher]
    (clojure.core/sequence xform (parser matcher))))

(defn cons
  [parser parsers]
  (fn [matcher]
    (clojure.core/cons (parser matcher) (parsers matcher))))

(defn pattern-parser [pat]
  (fn [matcher]
    (.usePattern ^java.util.regex.Matcher matcher pat)
    (when (.lookingAt ^java.util.regex.Matcher matcher)
      (let [res (.group ^java.util.regex.Matcher matcher 0)]
        (.region ^java.util.regex.Matcher matcher
                 (.end ^java.util.regex.Matcher matcher)
                 (.regionEnd ^java.util.regex.Matcher matcher))
        res))))
