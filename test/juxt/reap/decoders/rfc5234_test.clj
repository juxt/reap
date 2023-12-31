;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc5234-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.decoders.rfc5234 :as rfc5234]))

;; Normalization assumes sorted but not disjoint

(deftest normalize-test
  (is (= (list) (rfc5234/normalize [])))
  (is (= (list 10) (rfc5234/normalize [10])))
  (is (= (list #juxt.reap/interval [8 10] #juxt.reap/interval [12 20])
         (rfc5234/normalize [8 9 #juxt.reap/interval [9 10] 12 13 #juxt.reap/interval [14 20]])))
    ;; Can't get this working!!!
  ;;(thrown? clojure.lang.ExceptionInfo (normalize [[\T \Z] \A]))
  )

;; Should fail
;;(normalize [#juxt.reap/interval [\T \Z] \A])

;; Should be ok
;;(normalize [\A #juxt.reap/interval [\T \Z]])


(deftest alternatives-test
  (is (= [#juxt.reap/interval [1 10] #juxt.reap/interval [14 26]]
         (rfc5234/merge-alternatives
          #{#juxt.reap/interval [1 10] #juxt.reap/interval [14 20]}
          #{#juxt.reap/interval [18 26]}))))
