;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.graphql.util
  (:require
   [clojure.walk :refer [postwalk]]))

(defn lookup-fragment [doc fragment-name]
  (or
   (some #(when (= (get % :fragment-name) fragment-name) %) doc)
   (throw (ex-info "No fragment found" {:fragment-name fragment-name}))))

(defn deref-fragments
  [node doc]
  (postwalk
   (fn [x]
     (cond
       (and (map-entry? x) (= (first x) :selection-set))
       [:selection-set
        (vec
         (mapcat
          (fn [field-or-fragment]
            (if (= (first field-or-fragment) :fragment-spread)
              (:selection-set
               (deref-fragments
                (lookup-fragment doc (:fragment-name (second field-or-fragment)))
                doc))
              [field-or-fragment])) (second x)))]
       :else x))
   node))
