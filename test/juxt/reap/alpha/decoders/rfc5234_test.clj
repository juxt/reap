;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc5234-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.reap.alpha.decoders.rfc5234 :as rfc5234]))

;; Normalization assumes sorted but not disjoint

(deftest normalize-test
  (is (= (list) (rfc5234/normalize [])))
  (is (= (list 10) (rfc5234/normalize [10])))
  (is (= (list #juxt.reap.alpha/interval [8 10] #juxt.reap.alpha/interval [12 20])
         (rfc5234/normalize [8 9 #juxt.reap.alpha/interval [9 10] 12 13 #juxt.reap.alpha/interval [14 20]])))
    ;; Can't get this working!!!
  ;;(thrown? clojure.lang.ExceptionInfo (normalize [[\T \Z] \A]))
  )

;; Should fail
;;(normalize [#juxt.reap.alpha/interval [\T \Z] \A])

;; Should be ok
;;(normalize [\A #juxt.reap.alpha/interval [\T \Z]])


(deftest alternatives-test
  (is (= [#juxt.reap.alpha/interval [1 10] #juxt.reap.alpha/interval [14 26]]
         (rfc5234/merge-alternatives
          #{#juxt.reap.alpha/interval [1 10] #juxt.reap.alpha/interval [14 20]}
          #{#juxt.reap.alpha/interval [18 26]}))))
