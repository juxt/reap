;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc7232
  (:refer-clojure :exclude [type])
  (:require
   [juxt.reap.combinators :as p]
   [juxt.reap.interval :as i]
   [juxt.reap.regex :as re]
   [juxt.reap.rfc7232 :as rfc]
   [juxt.reap.rfc5234 :as rfc5234]
   [juxt.reap.decoders.rfc5234 :refer [DQUOTE alternatives]]
   [juxt.reap.decoders.rfc7230 :refer [OWS obs-text]]
   [juxt.reap.decoders.rfc7231 :refer [http-date]]))

(set! *warn-on-reflection* true)

(declare entity-tag)
(declare etagc)
(declare weak)

;; ETag = entity-tag
(defn etag [opts]
  (let [entity-tag (entity-tag opts)]
    (p/complete
     entity-tag)))

;; HTTP-date = <HTTP-date, see [RFC7231], Section 7.1.1.1>

;; If-Match = "*" / ( *( "," OWS ) entity-tag *( OWS "," [ OWS
;;  entity-tag ] ) )
(defn if-match [opts]
  (let [entity-tag (entity-tag opts)]
    (p/complete
     (p/alternatives
      (p/array-map
       ::rfc/wildcard
       (p/pattern-parser (re-pattern "\\*")))
      (p/first
       (p/sequence-group
        (p/ignore
         (p/pattern-parser
          (re-pattern (re/re-compose "(?:%s%s)*" \, OWS))))
        (p/cons
         entity-tag
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
               entity-tag)))))))))))))

;; If-Modified-Since = HTTP-date
(defn if-modified-since [opts]
  (let [http-date (http-date opts)]
    (p/complete http-date)))

;; If-None-Match = "*" / ( *( "," OWS ) entity-tag *( OWS "," [ OWS
;;  entity-tag ] ) )
(defn if-none-match [opts]
  (let [if-none-match (if-match opts)]
    (p/complete if-none-match)))

;; If-Unmodified-Since = HTTP-date
(defn if-unmodified-since [opts]
  (let [http-date (http-date opts)]
    (p/complete http-date)))

;; Last-Modified = HTTP-date
(defn last-modified [opts]
  (let [http-date (http-date opts)]
    (p/complete http-date)))

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; entity-tag = [ weak ] opaque-tag
(defn entity-tag [_]
  (p/comp
   (fn [x] (update x ::rfc/weak? some?))
   (p/pattern-parser
    (re-pattern (re/re-compose "(?<weak>%s)?(?<tag>%s[%s]*%s)" weak DQUOTE etagc DQUOTE))
    {:group {::rfc/weak? "weak"
             ::rfc/opaque-tag "tag"}})))

;; etagc = "!" / %x23-7E ; '#'-'~'
;;  / obs-text
(def ^{:type ::rfc5234/alternatives}
  etagc
  (alternatives
   \!
   (i/->interval [0x23 0x7E])
   obs-text))

;; obs-text = <obs-text, see [RFC7230], Section 3.2.6>

;; opaque-tag = DQUOTE *etagc DQUOTE
(def opaque-tag
  (re/re-compose "%s(?:[%s])*%s" DQUOTE etagc DQUOTE))

;; weak = %x57.2F ; W/
(def weak (str \W \/))
