;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.combinators
  (:refer-clojure
   :exclude [comp sequence cons concat filter
             list constantly first second map keep seq
             apply merge array-map some contains? into])
  (:require
   [clojure.core :as cc]
   [clojure.string :as str])
  (:import [java.util.regex Pattern Matcher]))

(set! *warn-on-reflection* true)

;; RFC 5234 Section 3.2. Alternatives: Rule1 / Rule2
(defn alternatives [& parsers]
  (assert (every? some? parsers))
  (fn [^java.util.regex.Matcher matcher]
    (let [start (.regionStart matcher)
          end (.regionEnd matcher)]
      (cc/some
       (fn [p]
         (p (doto matcher (.region start end)))) parsers))))

;; RFC 5234 Section 3.3. Incremental Alternatives: Rule1 =/ Rule2

;; NOTE: This can be achieved by using alternatives recursively

;; RFC 5234 Section 3.5. Sequence Group: (Rule1 Rule2)

(defn sequence-group
  "Create a parser that matches sequentially on each of the arguments."
  [& parsers]
  (assert (every? some? parsers))
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
    (when-let [fst (parser matcher)]
      (let [rst (parsers matcher)]
        (if (not= fst :ignore)
          (cc/cons fst rst)
          (when (seq rst)
            rst))))))

(defn constantly [constant]
  (fn [_] constant))

(defn ignore
  "Wrap the parser to allow a nil production, When a certain parser is optional,
  or its value is not useful enough to be emitted (i.e. it should be ignored),
  returning nil causes the top-level parser to terminate a branch. Instead, wrap
  the parser with this function to replace the result with the special :ignore
  keyword. Other combinators treat this keyword specially."
  [parser]
  (fn [matcher]
    (when (some? (parser matcher)) :ignore)))

(defn unignore
  "To prevent allowing the :ignore keyword to escape and become the result of
  evaluating an input with a parser, wrap in this function which will convert
  the special :ignore keyword to nil."
  [parser]
  (fn [matcher]
    (let [result (parser matcher)]
      (if (= result :ignore)
        nil
        result))))

(defn expect [message parser]
  (fn [^Matcher matcher]
    (let [result (parser matcher)]
      (if (some? result)
        result
        (throw
         (ex-info
          message
          {:matcher matcher
           :message message
           :start (.regionStart matcher)
           :end (.regionEnd matcher)}))))))

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

(defn string-literal [s]
  (pattern-parser
   (re-pattern
    (format "\\Q%s\\E" s))))

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

(defn keep [f parser]
  (fn [matcher]
    (cc/keep f (parser matcher))))

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
  (let [p (cc/apply sequence-group parsers)]
    (fn [matcher]
      (cc/apply cc/merge (p matcher)))))

(defn apply [p parsers]
  (let [parser (cc/apply p parsers)]
    (fn [matcher]
      (parser matcher))))

;; Functional

(defn complete
  "Declare that the production must match the input entirely. This is useful for
  top-level productions which must defend against extraneous input."
  [parser]
  (fn [matcher]
    (let [_parser
          (first
           (sequence-group
            parser
            ;; We don't mind extraneous whitespace, so we'll let that match
            (ignore (pattern-parser #"\s*"))))
          result (parser matcher)]
      (when (and result (not= result :ignore))
        (if (>= (.regionStart ^Matcher matcher) (.regionEnd ^Matcher matcher))
          result
          (do
            (.usePattern ^Matcher matcher #".*")
            (if (.find ^Matcher matcher)
              (let [remainder (.group ^Matcher matcher)]
                (throw
                 (ex-info
                  "Extraneous input"
                  {:matcher matcher
                   :result result
                   :remainder (let [limit 10]
                                (cond-> (subs remainder 0 (min (count remainder) limit))
                                  (> (count remainder) limit) (str "…")))})))
              (throw
               (ex-info
                "Extraneous input"
                {:matcher matcher
                 :result result})))))))))

;; Transformers

(defn lower-case [opts parser]
  (fn [matcher]
    (when-let [s (parser matcher)]
      (cond-> s
        (not (:juxt.reap/decode-preserve-case opts)) str/lower-case))))

(defn as-long [parser]
  (comp #(Long/parseLong %) parser))
