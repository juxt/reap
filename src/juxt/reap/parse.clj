;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.parse
  (:refer-clojure :exclude [comp sequence cons concat filter list constantly first second map]))

(set! *warn-on-reflection* true)

;; RFC 5234 Section 3.2. Alternatives: Rule1 / Rule2
(def alternatives some-fn)

;; RFC 5234 Section 3.3. Incremental Alternatives: Rule1 =/ Rule2

;; NOTE: This can be achieved by using alternatives recursively

;; RFC 5234 Section 3.5. Sequence Group: (Rule1 Rule2)
(defn sequence-group
  "Create a parser that matches sequentially on each of the arguments."
  [& parsers]
  (fn [matcher]
    (let [f (fn f [[p & ps]]
              (when p
                (lazy-seq
                 (when-let [x (p matcher)]
                   (clojure.core/cons x (f ps))))))
          res (f parsers)]
      (when (seq res)
        (remove #(= :ignore %) (vec res))))))

;; RFC 5234 Section 3.6: Variable Repetition
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
    (let [cell (parser matcher)
          rest (parsers matcher)]
      (if (= cell :ignore)
        rest
        (clojure.core/cons cell rest)))))

(defn list
  [& parsers]
  (fn [matcher]
    (doall (clojure.core/map #(% matcher) parsers))))

(defn concat [& parsers]
  (fn [matcher]
    (doall
     (apply
      clojure.core/concat
      (clojure.core/map #(% matcher) parsers)))))

(defn constantly [constant]
  (fn [_] constant))

(defn ignore [parser]
  (fn [matcher]
    (when (some? (parser matcher)) :ignore)))

(defn pattern-parser [pat]
  (fn [matcher]
    (.usePattern ^java.util.regex.Matcher matcher pat)
    (when (.lookingAt ^java.util.regex.Matcher matcher)
      (let [res (.group ^java.util.regex.Matcher matcher 0)]
        (.region ^java.util.regex.Matcher matcher
                 (.end ^java.util.regex.Matcher matcher)
                 (.regionEnd ^java.util.regex.Matcher matcher))
        res))))

(defn first [parser]
  (fn [matcher]
    (clojure.core/first (parser matcher))))

(defn second [parser]
  (fn [matcher]
    (clojure.core/second (parser matcher))))

(defn filter [pred parser]
  (fn [matcher]
    (clojure.core/filter pred (parser matcher))))

(defn map [f parser]
  (fn [matcher]
    (clojure.core/map f (parser matcher))))
