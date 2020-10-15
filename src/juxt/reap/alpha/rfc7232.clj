;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7232
  (:refer-clojure :exclude [type])
  (:require
   [juxt.reap.alpha.interval :as i]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.rfc5234 :as rfc5234]
   [juxt.reap.alpha.rfc7230 :refer [OWS obs-text]]
   [juxt.reap.alpha.rfc7231 :refer [http-date]]))

(set! *warn-on-reflection* true)

(declare entity-tag)
(declare etagc)
(declare weak)

;; ETag = entity-tag
(def etag entity-tag)

;; HTTP-date = <HTTP-date, see [RFC7231], Section 7.1.1.1>

;; If-Match = "*" / ( *( "," OWS ) entity-tag *( OWS "," [ OWS
;;  entity-tag ] ) )
(defn if-match ^:juxt.reap/codec [opts]
  (let [entity-tag (entity-tag opts)]
    {:juxt.reap/decode
     (p/alternatives
      (p/array-map
       :juxt.http/wildcard
       (p/pattern-parser (re-pattern "\\*")))
      (p/first
       (p/sequence-group
        (p/ignore
         (p/pattern-parser
          (re-pattern (re/re-compose "(?:%s%s)*" \, OWS))))
        (p/cons
         (p/array-map
          :juxt.http/entity-tag
          (:juxt.reap/decode entity-tag))
         (p/zero-or-more
          (p/first
           (p/sequence-group
            (p/ignore
             (p/pattern-parser
              (re-pattern
               (re/re-compose "%s%s" OWS ","))))
            (p/optionally
             (p/first
              (p/sequence-group
               (p/ignore
                (p/pattern-parser (re-pattern OWS)))
               (p/array-map
                :juxt.http/entity-tag
                (:juxt.reap/decode entity-tag))))))))))))}))

;; If-Modified-Since = HTTP-date
(def if-modified-since http-date)

;; If-None-Match = "*" / ( *( "," OWS ) entity-tag *( OWS "," [ OWS
;;  entity-tag ] ) )
(defn if-none-match ^:juxt.reap/codec [opts]
  {:juxt.reap/decode (:juxt.reap/decode (if-match opts))})

(comment
  [((:juxt.reap/decode (if-none-match {})) (re/input "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\""))
   ((:juxt.reap/decode (if-none-match {})) (re/input "\"xyzzy\""))
   ((:juxt.reap/decode (if-none-match {})) (re/input "*"))])

;; If-Unmodified-Since = HTTP-date
(def if-unmodified-since http-date)

;; Last-Modified = HTTP-date
(def last-modified http-date)

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; entity-tag = [ weak ] opaque-tag
(defn entity-tag ^:juxt.reap/codec [opts]
  {:juxt.reap/decode
   (p/comp
    (fn [x] (update x :weak? some?))
    (p/pattern-parser
     (re-pattern (re/re-compose "((?<weak>%s)?%s(?<tag>[%s]*)%s)" weak rfc5234/DQUOTE etagc rfc5234/DQUOTE))
     {:group {:weak? "weak"
              :entity-tag 0}}))})

(comment
  ((:juxt.reap/decode (entity-tag {}))
   (re/input "W/\"xyzzy\"")))

(comment
  ((:juxt.reap/decode (entity-tag {}))
   (re/input "\"\"")))

;; etagc = "!" / %x23-7E ; '#'-'~'
;;  / obs-text
(def ^{:type ::rfc5234/alternatives}
  etagc
  (rfc5234/alternatives
   \!
   (i/->interval [0x23 0x7E])
   obs-text))

;; obs-text = <obs-text, see [RFC7230], Section 3.2.6>

;; opaque-tag = DQUOTE *etagc DQUOTE
(def opaque-tag
  (re/re-compose "%s(?:[%s])*%s" rfc5234/DQUOTE etagc rfc5234/DQUOTE))

;; weak = %x57.2F ; W/
(def weak (str \W \/))
