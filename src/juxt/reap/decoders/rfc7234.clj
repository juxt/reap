;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc7234
  (:require
   [juxt.reap.combinators :as p]
   [juxt.reap.regex :as re]
   [juxt.reap.rfc7234 :as rfc]
   [juxt.reap.decoders.rfc5234 :refer [DIGIT]]
   [juxt.reap.decoders.rfc7230 :refer [OWS token quoted-string unescape-quoted-string]]
   [juxt.reap.decoders.rfc7231 :refer [http-date]]))

(set! *warn-on-reflection* true)

(declare cache-directive)
(declare delta-seconds)
(declare pragma-directive)

;; Accept-Ranges = acceptable-ranges
(defn age [_]
  (p/complete
   (p/pattern-parser
    (re-pattern delta-seconds))))

;; Cache-Control = *( "," OWS ) cache-directive *( OWS "," [ OWS
;;  cache-directive ] )
(defn cache-control [opts]
  (let [cache-directive (cache-directive opts)]
    (p/complete
     (p/first
      (p/sequence-group
       (p/ignore
        (p/pattern-parser
         (re-pattern
          (re/re-compose "(?:%s%s)*" \, OWS))))
       (p/cons
        cache-directive
        (p/keep
         first
         (p/zero-or-more
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern
              (re/re-compose "(?:%s%s)" OWS \,))))
           (p/optionally
            (p/first
             (p/sequence-group
              (p/ignore
               (p/pattern-parser (re-pattern OWS)))
              cache-directive))))))))))))

(defn expires [opts]
  (p/complete
   (http-date opts)))

;; cache-directive = token [ "=" ( token / quoted-string ) ]
(defn cache-directive
  [_]
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     ::rfc/cache-directive
     (p/pattern-parser
      (re-pattern token)))
    (p/optionally
     (p/first
      (p/sequence-group
       (p/ignore (p/pattern-parser #"="))
       (p/as-entry
        ::rfc/cache-directive-value
        (p/alternatives
         (p/pattern-parser (re-pattern token))
         (p/comp
          unescape-quoted-string
          (p/pattern-parser
           (re-pattern quoted-string) {:group 1}))))))))))

;; delta-seconds = 1*DIGIT
(def delta-seconds (re/re-compose "%s+" DIGIT))
