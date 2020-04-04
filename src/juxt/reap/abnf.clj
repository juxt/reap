(ns juxt.reap.abnf
  (:require
   [clojure.string :as string]
   [juxt.reap.parse :as p]
   [juxt.reap.regex :as re]
   [juxt.reap.rfc5234 :refer [ALPHA DIGIT]])
  )

;; (identity ALPHA)
;; (contains? (ns-refers *ns*) 'ALPHA)

(string/split
 "*( \",\" OWS ) field-name *( OWS \",\" [ OWS field-name ] )"
 #"\s+"
 )

;; Accept-Charset = *( "," OWS ) ( ( charset / "*" ) [ weight ] ) *( OWS
;;  "," [ OWS ( ( charset / "*" ) [ weight ] ) ] )

(do
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
              (sequence-group
               new-input
               [[:alternatives (apply vector :sequence-group results) sub-results]]
               depth)))

          #"\"([^\"]+)\""
          :>>
          (fn [[_ pattern]]
            (sequence-group
             input
             (conj results [:pattern-parser pattern])
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

  (sequence-group
   (string/split
    "*( \",\" OWS )"
    #"\s+")
   [] 0


   )

  #_(sequence-group
   (string/split
    "*( \",\" OWS ) ( ( charset / \"*\" ) [ weight ] ) *( OWS \",\" [ OWS ( ( charset / \"*\" ) [ weight ] ) ] )"
    #"\s+")
   []
   ))
