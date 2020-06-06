;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.combinators
  (:refer-clojure :exclude [comp sequence cons concat filter list constantly first second map seq apply merge array-map])
  (:import [java.util.regex Pattern Matcher]))

(set! *warn-on-reflection* true)

;; RFC 5234 Section 3.2. Alternatives: Rule1 / Rule2
(defn alternatives [& parsers]
  (fn [matcher]
    (some (fn [p] (p matcher)) parsers)))

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
              (clojure.core/cons match (this matcher)))))]
    (fn [matcher]
      (or (super matcher) '()))))

(defn seq [parser]
  (fn [matcher]
    (let [res (parser matcher)]
      (if (= res :ignore) res (or (clojure.core/seq res) :ignore)))))

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
    (clojure.core/sequence xform (parser matcher))))

(defn cons
  [parser parsers]
  (fn [matcher]
    (if-let [fst (parser matcher)]
      (let [rst (parsers matcher)]
        (if (not= fst :ignore)
          (clojure.core/cons fst rst)
          rst))
      '())))

(defn list
  [& parsers]
  (fn [matcher]
    (doall (clojure.core/map #(% matcher) parsers))))

(defn concat [& parsers]
  (fn [matcher]
    (doall
     (clojure.core/apply
      clojure.core/concat
      (clojure.core/map #(% matcher) parsers)))))

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
       (let [res (if-let [grp (:group opts)]
                   (cond
                     (int? grp)
                     (.group ^Matcher matcher ^long grp)
                     (string? grp)
                     (.group ^Matcher matcher ^String grp)
                     (map? grp)
                     (reduce
                      (fn [acc [k v]]
                        (let [vl (cond
                                   (int? v)
                                   (.group ^Matcher matcher ^long v)
                                   (string? v)
                                   (.group ^Matcher matcher ^String v)
                                   :else
                                   (throw (ex-info "Bad map value group type")))]
                          (cond-> acc vl (conj [k vl]))))
                      {} grp)


                     :else
                     (throw (ex-info "Bad group type")))
                   (re-groups matcher))]
         (.region ^Matcher matcher
                  (.end ^Matcher matcher)
                  (.regionEnd ^Matcher matcher))
         res)))))

(defn first [parser]
  (fn [matcher]
    (clojure.core/first
     (parser matcher))))

(defn second [parser]
  (fn [matcher]
    (clojure.core/second (parser matcher))))

(defn filter [pred parser]
  (fn [matcher]
    (clojure.core/filter pred (parser matcher))))

(defn map [f parser]
  (fn [matcher]
    (clojure.core/map f (parser matcher))))

;; Data construction
(defn as-entry [k parser]
  (fn [matcher]
    (when-let [v (parser matcher)]
      (if (= v :ignore)
        :ignore
        [k v]))))

(defn array-map [& keyvals]
  (fn [matcher]
    (clojure.core/apply
     clojure.core/array-map
     (clojure.core/mapcat
      (fn [[k v]] [k (v matcher)]) (clojure.core/partition 2 keyvals)))))

(defn as-map [parser]
  (fn [matcher]
    (let [res (parser matcher)]
      (when res
        (into {} res)))))

(defn merge [& parsers]
  (let [p (sequence-group parsers)]
    (fn [matcher]
      (clojure.core/apply clojure.core/merge (p matcher)))))

(defn apply [p parsers]
  (let [parser (clojure.core/apply p parsers)]
    (fn [matcher]
      (parser matcher))))
