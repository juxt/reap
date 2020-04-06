;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.abnf
  (:require
   [clojure.string :as string]
   [juxt.reap.parse :as p]
   [juxt.reap.regex :as re]))

;; (identity ALPHA)
;; (contains? (ns-refers *ns*) 'ALPHA)

(defn sequence-group [input results depth]
  (let [[tok & input] input]
    (if tok
      (condp re-matches tok

        ;; TODO: Check type of sequence group
        #"\)" {:input input
               :results (apply vector :sequence-group results)}

        ;; TODO: Check type of sequence group
        #"\]" {:input input
               :results results}

        #"\*\("
        :>>
        (fn [_]
          (let [{new-input :input
                 sub-results :results}
                (sequence-group input [] (inc depth))]
            (sequence-group
             new-input
             (conj
              results
              [:zero-or-more
               sub-results])
             depth)))

        #"\("
        :>>
        (fn [_]
          (let [{new-input :input
                 sub-results :results}
                (sequence-group input [] (inc depth))]
            (sequence-group
             new-input
             (conj results sub-results)
             depth)))

        #"\["
        :>>
        (fn [_]
          (let [{new-input :input
                 sub-results :results}
                (sequence-group input [] (inc depth))]
            (sequence-group
             new-input
             (conj results [:optionally (apply vector :sequence-group sub-results)])
             depth)))

        #"/"
        :>>
        (fn [_]
          (let [{new-input :input
                 sub-results :results}
                (sequence-group input [] depth)]
            {:input new-input
             :results (apply vector :alternatives (conj results sub-results))}))

        #"\"([^\"]+)\""
        :>>
        (fn [[_ pattern]]
          (sequence-group
           input
           (conj results [:pattern pattern])
           depth))

        #"[A-Za-z-]+"
        :>>
        (fn [x]
          (sequence-group
           input
           (conj results [:non-terminal x])
           depth)))

      ;; No more toks, return results

      (if (zero? depth)
        {:input input
         :results (apply vector :sequence-group results)}
        (throw (ex-info "Unexpected end of input" {:depth depth})))
      )))

;; Accept-Charset = *( "," OWS ) ( ( charset / "*" ) [ weight ] ) *( OWS
;;  "," [ OWS ( ( charset / "*" ) [ weight ] ) ] )


(defn parse-abnf [s]
  (let [{:keys [input results]}
        (sequence-group
         (string/split
          s
          #"\s+")
         [] 0)]
    (assert (nil? input))
    results))

(defn entry? [node]
  (and (vector? node) (= (count node) 2) (keyword (first node))))

(defmulti replace-branch
  (fn [x]
    (if (vector? x) (first x) :default)))

(defmethod replace-branch :default [x] x)

(defmethod replace-branch :pattern [[_ s]]
  (p/pattern-parser
   (re-pattern
    (format "\\Q%s\\E" s))))

(defmethod replace-branch :non-terminal [[_ x]]
  (if-let [f (get (ns-refers *ns*) (symbol x))]
    (let [p
          (if (fn? @f)
            (@f)
            (p/pattern-parser (re-pattern (re/re-str @f))))]
      (cond->
          (p/as-entry (keyword x) p)
        (#{"OWS"} x) p/ignore
        )
      )
    (throw (ex-info "Non-terminal not found" {:non-terminal x}))))

(defn process-sequence-group [res]
  (if (sequential? res)
    (cond (empty? res) nil
          (= (count res) 1) (first res)
          (some entry? res) (into {} (filter entry? res))
          (some map? res) (filter (some-fn map? sequential?) res)
          :else (filter (some-fn map? sequential?) res))
    res))

(defmethod replace-branch :sequence-group [[_ & clauses]]
  (p/comp
   (fn [res]
     (let [new-res
           (process-sequence-group res)]
       (prn res "->" new-res)
       new-res))
   (apply p/sequence-group clauses)))

(defmethod replace-branch :optionally [[_ clause]]
  (p/optionally clause))

(defmethod replace-branch :alternatives [[_ & clauses]]
  (apply p/alternatives clauses))

(defmethod replace-branch :zero-or-more [[_ & clauses]]
  (p/comp vec (apply p/zero-or-more clauses)))


#_(prewalk
 (fn [node]
   (cond
     (entry? node) node
     (and (sequential? node) (some entry? node)) (into {} (filter entry? node))
     (map? node) node
     (and (sequential? node) (some map? node)) node
     (sequential? node) node
     :else node
     )

   )

 [[[","] [","] [","] [","]]
  [[:charset "foo"] [[:weight 0.8]]]
  [["," [[[:charset "utf-8"]]]]
   ["," [[[:charset "utf-16"] [[:weight 0.1]]]]]]])
