;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.interval-test
  (:refer-clojure :exclude [contains?])
  (:require
   [clojure.test :refer [deftest are is]]
   [juxt.reap.alpha.interval :as i]))

(deftest interval-test
  (are [a _ e] (= e a)
    (i/beginning #juxt.reap.alpha/interval [\A \Z]) :=> (int \A)
    (i/end #juxt.reap.alpha/interval [\A \Z]) :=> (int \Z)
    ;; Characters behave a single element ranges
    (i/beginning \A) :=> 65
    (i/end \Z) :=> 90
    ;; Integers behave a single element ranges
    (i/beginning 10) :=> 10
    (i/end 20) :=> 20))

(deftest print-interval-test
  (is (= "%x0A-%x14" (print-str #juxt.reap.alpha/interval [10 20])))
  (is (= "#juxt.reap.alpha/interval [10 20]" (pr-str #juxt.reap.alpha/interval [10 20]))))

(deftest relations-test

  (are [x rel y] (= rel (i/relation x y))
    #juxt.reap.alpha/interval [10 20] :precedes #juxt.reap.alpha/interval [30 40]
    7 :precedes 10
    7 :precedes #juxt.reap.alpha/interval [10 20])

  #juxt.reap.alpha/interval [10 20] :precedes 30

  #juxt.reap.alpha/interval [10 20] :meets #juxt.reap.alpha/interval [21 40]
  10 :meets 11
  #juxt.reap.alpha/interval [10 20] :meets 21
  9 :meets #juxt.reap.alpha/interval [10 20]

  #juxt.reap.alpha/interval [10 15] :starts #juxt.reap.alpha/interval [10 20]
  10 :starts #juxt.reap.alpha/interval [10 15]

  #juxt.reap.alpha/interval [10 13] :during #juxt.reap.alpha/interval [8 20]
  10 :during #juxt.reap.alpha/interval [8 20]

  #juxt.reap.alpha/interval [12 15] :finishes #juxt.reap.alpha/interval [10 15]
  15 :finishes #juxt.reap.alpha/interval [10 15]

  #juxt.reap.alpha/interval [10 20] :overlaps #juxt.reap.alpha/interval [15 30]

  #juxt.reap.alpha/interval [10 20] :equals #juxt.reap.alpha/interval [10 20]
  10 :equals 10

  #juxt.reap.alpha/interval [8 20] :contains #juxt.reap.alpha/interval [10 13]
  #juxt.reap.alpha/interval [8 20] :contains 10

  #juxt.reap.alpha/interval [10 20] :started-by #juxt.reap.alpha/interval [10 15]
  #juxt.reap.alpha/interval [10 15] :started-by 10

  #juxt.reap.alpha/interval [10 15] :finished-by #juxt.reap.alpha/interval [12 15]
  #juxt.reap.alpha/interval [10 15] :finished-by 15

  #juxt.reap.alpha/interval [15 30] :overlapped-by #juxt.reap.alpha/interval [10 20]

  #juxt.reap.alpha/interval [21 40] :met-by #juxt.reap.alpha/interval [10 20]
  11 :met-by 10
  21 :met-by #juxt.reap.alpha/interval [10 20]
  #juxt.reap.alpha/interval [10 20] :met-by 9

  #juxt.reap.alpha/interval [30 40] :preceded-by #juxt.reap.alpha/interval [10 20]
  10 :preceded-by 7
  #juxt.reap.alpha/interval [10 20] :preceded-by 7
  30 :preceded-by #juxt.reap.alpha/interval [10 20])


(deftest joinable-test
  ;; Comprehensively including all 13 relations
  (are [x rel y expected]
      (and
       (= rel (i/relation x y))
       (case expected :joinable (i/joinable? x y) :not-joinable (not (i/joinable? x y))))
    ;; Cannot join two intervals that have a gap between them
    #juxt.reap.alpha/interval [10 20] :precedes #juxt.reap.alpha/interval [30 40] :not-joinable
    #juxt.reap.alpha/interval [10 20] :meets #juxt.reap.alpha/interval [21 40] :joinable
    #juxt.reap.alpha/interval [10 15] :starts #juxt.reap.alpha/interval [10 20] :joinable
    ;; Wrong ordering
    #juxt.reap.alpha/interval [10 13] :during #juxt.reap.alpha/interval [8 20] :not-joinable
    ;; Wrong ordering
    #juxt.reap.alpha/interval [12 15] :finishes #juxt.reap.alpha/interval [10 15] :not-joinable
    #juxt.reap.alpha/interval [10 20] :overlaps #juxt.reap.alpha/interval [15 30] :joinable
    #juxt.reap.alpha/interval [10 20] :equals #juxt.reap.alpha/interval [10 20] :joinable
    #juxt.reap.alpha/interval [8 20] :contains #juxt.reap.alpha/interval [10 13] :joinable
    #juxt.reap.alpha/interval [10 20] :started-by #juxt.reap.alpha/interval [10 15] :joinable
    #juxt.reap.alpha/interval [10 15] :finished-by #juxt.reap.alpha/interval [12 15] :joinable
    ;; Wrong ordering
    #juxt.reap.alpha/interval [15 30] :overlapped-by #juxt.reap.alpha/interval [10 20] :not-joinable
    ;; Wrong ordering
    #juxt.reap.alpha/interval [21 40] :met-by #juxt.reap.alpha/interval [10 20] :not-joinable
    ;; Wrong ordering
    #juxt.reap.alpha/interval [30 40] :preceded-by #juxt.reap.alpha/interval [10 20] :not-joinable))

(deftest join-test
  (are [x y _ expected] (= expected (i/join x y))
    10 11 :=> #juxt.reap.alpha/interval [10 11]
    #juxt.reap.alpha/interval [10 11] #juxt.reap.alpha/interval [12 13] :=> #juxt.reap.alpha/interval [10 13]
    #juxt.reap.alpha/interval [8 10] 11 :=> #juxt.reap.alpha/interval [8 11]
    10 #juxt.reap.alpha/interval [11 13] :=> #juxt.reap.alpha/interval [10 13]))
