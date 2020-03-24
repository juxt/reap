;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.interval-test
  (:refer-clojure :exclude [contains?])
  (:require
   [clojure.test :refer [deftest are is]]
   [juxt.reap.interval :as i]))

(deftest interval-test
  (are [a _ e] (= e a)
    (i/beginning #juxt.reap/interval [\A \Z]) :=> (int \A)
    (i/end #juxt.reap/interval [\A \Z]) :=> (int \Z)
    ;; Characters behave a single element ranges
    (i/beginning \A) :=> 65
    (i/end \Z) :=> 90
    ;; Integers behave a single element ranges
    (i/beginning 10) :=> 10
    (i/end 20) :=> 20))

(deftest print-interval-test
  (is (= "%x0A-%x14" (print-str #juxt.reap/interval [10 20])))
  (is (= "#juxt.reap/interval [10 20]" (pr-str #juxt.reap/interval [10 20]))))

(deftest relations-test

  (are [x rel y] (= rel (i/relation x y))
    #juxt.reap/interval [10 20] :precedes #juxt.reap/interval [30 40]
    7 :precedes 10
    7 :precedes #juxt.reap/interval [10 20])

  #juxt.reap/interval [10 20] :precedes 30

  #juxt.reap/interval [10 20] :meets #juxt.reap/interval [21 40]
  10 :meets 11
  #juxt.reap/interval [10 20] :meets 21
  9 :meets #juxt.reap/interval [10 20]

  #juxt.reap/interval [10 15] :starts #juxt.reap/interval [10 20]
  10 :starts #juxt.reap/interval [10 15]

  #juxt.reap/interval [10 13] :during #juxt.reap/interval [8 20]
  10 :during #juxt.reap/interval [8 20]

  #juxt.reap/interval [12 15] :finishes #juxt.reap/interval [10 15]
  15 :finishes #juxt.reap/interval [10 15]

  #juxt.reap/interval [10 20] :overlaps #juxt.reap/interval [15 30]

  #juxt.reap/interval [10 20] :equals #juxt.reap/interval [10 20]
  10 :equals 10

  #juxt.reap/interval [8 20] :contains #juxt.reap/interval [10 13]
  #juxt.reap/interval [8 20] :contains 10

  #juxt.reap/interval [10 20] :started-by #juxt.reap/interval [10 15]
  #juxt.reap/interval [10 15] :started-by 10

  #juxt.reap/interval [10 15] :finished-by #juxt.reap/interval [12 15]
  #juxt.reap/interval [10 15] :finished-by 15

  #juxt.reap/interval [15 30] :overlapped-by #juxt.reap/interval [10 20]

  #juxt.reap/interval [21 40] :met-by #juxt.reap/interval [10 20]
  11 :met-by 10
  21 :met-by #juxt.reap/interval [10 20]
  #juxt.reap/interval [10 20] :met-by 9

  #juxt.reap/interval [30 40] :preceded-by #juxt.reap/interval [10 20]
  10 :preceded-by 7
  #juxt.reap/interval [10 20] :preceded-by 7
  30 :preceded-by #juxt.reap/interval [10 20])


(deftest spliceable-test
  ;; Comprehensively including all 13 relations
  (are [x rel y expected]
      (and
       (= rel (i/relation x y))
       (case expected :spliceable (i/spliceable? x y) :not-spliceable (not (i/spliceable? x y))))
    ;; Cannot splice two intervals that have a gap between them
    #juxt.reap/interval [10 20] :precedes #juxt.reap/interval [30 40] :not-spliceable
    #juxt.reap/interval [10 20] :meets #juxt.reap/interval [21 40] :spliceable
    #juxt.reap/interval [10 15] :starts #juxt.reap/interval [10 20] :spliceable
    ;; Wrong ordering
    #juxt.reap/interval [10 13] :during #juxt.reap/interval [8 20] :not-spliceable
    ;; Wrong ordering
    #juxt.reap/interval [12 15] :finishes #juxt.reap/interval [10 15] :not-spliceable
    #juxt.reap/interval [10 20] :overlaps #juxt.reap/interval [15 30] :spliceable
    #juxt.reap/interval [10 20] :equals #juxt.reap/interval [10 20] :spliceable
    #juxt.reap/interval [8 20] :contains #juxt.reap/interval [10 13] :spliceable
    #juxt.reap/interval [10 20] :started-by #juxt.reap/interval [10 15] :spliceable
    #juxt.reap/interval [10 15] :finished-by #juxt.reap/interval [12 15] :spliceable
    ;; Wrong ordering
    #juxt.reap/interval [15 30] :overlapped-by #juxt.reap/interval [10 20] :not-spliceable
    ;; Wrong ordering
    #juxt.reap/interval [21 40] :met-by #juxt.reap/interval [10 20] :not-spliceable
    ;; Wrong ordering
    #juxt.reap/interval [30 40] :preceded-by #juxt.reap/interval [10 20] :not-spliceable))

(deftest splice-test
  (are [x y _ expected] (= expected (i/splice x y))
    10 11 :=> #juxt.reap/interval [10 11]
    #juxt.reap/interval [10 11] #juxt.reap/interval [12 13] :=> #juxt.reap/interval [10 13]
    #juxt.reap/interval [8 10] 11 :=> #juxt.reap/interval [8 11]
    10 #juxt.reap/interval [11 13] :=> #juxt.reap/interval [10 13]))
