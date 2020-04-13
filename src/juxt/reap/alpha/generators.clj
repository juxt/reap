;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.generators)

(defn histogram-generator
  "Given a collection of [val freq] pairs, where freq is the relative
  probability of selection, return a function with no args which
  selects a val from one of the pairs."
  [coll]
  (let [sum (reduce + (map second coll))]
    (fn []
      (let [n (rand-int sum)]
        (reduce
         (fn [tot [cand w]]
           (if (> (+ tot w) n) (reduced cand) (+ tot w)))
         0
         coll)))))
