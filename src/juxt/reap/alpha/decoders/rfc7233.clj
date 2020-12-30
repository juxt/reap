;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7233
  (:refer-clojure :exclude [range])
  (:require
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.rfc7232 :as rfc7232]
   [juxt.reap.alpha.rfc7233 :as rfc]
   [juxt.reap.alpha.decoders.rfc5234 :refer [CHAR DIGIT SP VCHAR]]
   [juxt.reap.alpha.decoders.rfc7230 :refer [OWS token]]
   [juxt.reap.alpha.decoders.rfc7231 :refer [http-date]]
   [juxt.reap.alpha.decoders.rfc7232 :refer [entity-tag]]))

(set! *warn-on-reflection* true)

(declare acceptable-ranges)
(declare byte-content-range)
(declare byte-range-resp)
(declare byte-range-spec)
(declare byte-ranges-specifier)
(declare bytes-unit)
(declare complete-length)
(declare first-byte-pos)
(declare last-byte-pos)
(declare other-content-range)
(declare other-range-resp)
(declare other-range-unit)
(declare other-ranges-specifier)
(declare range-unit)
(declare suffix-byte-range-spec)
(declare suffix-length)
(declare unsatisfied-range)

;; Accept-Ranges = acceptable-ranges
(def accept-ranges acceptable-ranges)

;; Content-Range = byte-content-range / other-content-range
(defn content-range [opts]
  (p/alternatives
   (byte-content-range opts)
   (other-content-range opts)))

;; HTTP-date = <HTTP-date, see [RFC7231], Section 7.1.1.1>

;; If-Range = entity-tag / HTTP-date
(defn if-range [opts]
  (let [entity-tag (entity-tag opts)
        http-date (http-date opts)]
    (p/alternatives
     (p/array-map ::rfc7232/entity-tag entity-tag)
     (p/array-map ::rfc7231/http-date http-date))))

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; Range = byte-ranges-specifier / other-ranges-specifier
(defn range [opts]
  (let [byte-ranges-specifier (byte-ranges-specifier opts)
        other-ranges-specifier (other-ranges-specifier opts)]
    (p/alternatives
     byte-ranges-specifier
     other-ranges-specifier)))

;; acceptable-ranges = ( *( "," OWS ) range-unit *( OWS "," [ OWS
;;  range-unit ] ) ) / "none"
(defn acceptable-ranges [opts]
  (let [range-unit (range-unit opts)]
    (p/alternatives
     (p/pattern-parser (re-pattern "none"))
     (p/first
      (p/sequence-group
       (p/ignore
        (p/pattern-parser
         (re-pattern (re/re-compose "(?:%s)*" (re/re-concat \, OWS)))))
       (p/cons
        range-unit
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern (re/re-compose "(?:%s)" (re/re-concat OWS \,)))))
           (p/optionally
            (p/first
             (p/sequence-group
              (p/ignore
               (p/pattern-parser
                (re-pattern OWS)))
              range-unit))))))))))))

;; byte-content-range = bytes-unit SP ( byte-range-resp / unsatisfied-range )
(defn byte-content-range [opts]
  (let [bytes-unit (bytes-unit opts)
        byte-range-resp (byte-range-resp opts)
        unsatisfied-range (unsatisfied-range opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry
       ::rfc/units
       bytes-unit)
      (p/ignore
       (p/pattern-parser
        (re-pattern
         (re/re-compose "%s" SP))))
      (p/alternatives
       byte-range-resp
       unsatisfied-range)))))


;; byte-range = first-byte-pos "-" last-byte-pos
(defn byte-range [opts]
  (let [last-byte-pos (last-byte-pos opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry ::rfc/first-byte-pos last-byte-pos)
      (p/ignore (p/pattern-parser #"\-"))
      (p/as-entry ::rfc/last-byte-pos last-byte-pos)))))

;; byte-range-resp = byte-range "/" ( complete-length / "*" )
(defn byte-range-resp [opts]
  (let [byte-range (byte-range opts)
        complete-length (complete-length opts)]
    (p/merge
     byte-range
     (p/ignore
      (p/pattern-parser #"\/"))
     (p/alternatives
      (p/array-map
       ::rfc/complete-length
       complete-length)
      (p/array-map
       ::rfc/wildcard
       (p/pattern-parser #"\*"))))))

;; byte-range-set = *( "," OWS ) ( byte-range-spec /
;;  suffix-byte-range-spec ) *( OWS "," [ OWS ( byte-range-spec /
;;  suffix-byte-range-spec ) ] )
(defn byte-range-set [opts]
  (let [byte-range-spec (byte-range-spec opts)
        suffix-byte-range-spec (suffix-byte-range-spec opts)]
    (p/first
     (p/sequence-group
      (p/ignore
       (p/pattern-parser
        (re-pattern
         (re/re-compose "(?:%s%s)*" "," OWS))))
      (p/cons
       (p/alternatives
        byte-range-spec
        suffix-byte-range-spec)
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
              (p/pattern-parser
               (re-pattern OWS)))
             (p/alternatives
              byte-range-spec
              suffix-byte-range-spec))))))))))))

;; byte-range-spec = first-byte-pos "-" [ last-byte-pos ]
(defn byte-range-spec [opts]
  (let [first-byte-pos (first-byte-pos opts)
        last-byte-pos (last-byte-pos opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry
       ::rfc/first-byte-pos
       first-byte-pos)
      (p/ignore
       (p/pattern-parser #"\-"))
      (p/optionally
       (p/as-entry
        ::rfc/last-byte-pos
        last-byte-pos))))))

;; byte-ranges-specifier = bytes-unit "=" byte-range-set
(defn byte-ranges-specifier [opts]
  (let [bytes-unit (bytes-unit opts)
        byte-range-set (byte-range-set opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry
       ::rfc/units
       bytes-unit)
      (p/ignore
       (p/pattern-parser #"\="))
      (p/optionally
       (p/as-entry
        ::rfc/byte-range-set
        byte-range-set))))))

;; bytes-unit = "bytes"
(defn bytes-unit [_]
  (p/pattern-parser (re-pattern "bytes")))

;; complete-length = 1*DIGIT
(defn complete-length [_]
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; entity-tag = <entity-tag, see [RFC7232], Section 2.3>

;; first-byte-pos = 1*DIGIT
(defn first-byte-pos [_]
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; last-byte-pos = 1*DIGIT
(defn last-byte-pos [_]
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; other-content-range = other-range-unit SP other-range-resp
(defn other-content-range [opts]
  (let [other-range-unit (other-range-unit opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry
       ::rfc/units
       other-range-unit)
      (p/ignore
       (p/pattern-parser
        (re-pattern
         (re/re-compose "%s" SP))))
      (p/as-entry
       ::rfc/range-resp
       (p/pattern-parser (re-pattern other-range-resp)))))))

;; other-range-resp = *CHAR
(def ^String other-range-resp (re/re-compose "%s*" CHAR))

;; other-range-set = 1*VCHAR
(def ^String other-range-set (re/re-compose "%s+" VCHAR))

;; other-range-unit = token
(defn other-range-unit [_]
  (p/pattern-parser (re-pattern token)))

;; other-ranges-specifier = other-range-unit "=" other-range-set
(defn other-ranges-specifier [opts]
  (let [other-range-unit (other-range-unit opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry
       ::rfc/units
       other-range-unit)
      (p/ignore
       (p/pattern-parser #"\="))
      (p/optionally
       (p/as-entry
        ::rfc/range-set
        (p/pattern-parser (re-pattern other-range-set))))))))

;; range-unit = bytes-unit / other-range-unit
(defn range-unit [opts]
  (let [bytes-unit (bytes-unit opts)
        other-range-unit (other-range-unit opts)]
    (p/alternatives
     bytes-unit
     other-range-unit)))

;; suffix-byte-range-spec = "-" suffix-length
(defn suffix-byte-range-spec [opts]
  (let [suffix-length (suffix-length opts)]
    (p/into
     {}
     (p/sequence-group
      (p/ignore
       (p/pattern-parser #"\-"))
      (p/as-entry
       ::rfc/suffix-length
       suffix-length)))))

;; suffix-length = 1*DIGIT
(defn suffix-length [_]
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; token = <token, see [RFC7230], Section 3.2.6>

;; unsatisfied-range = "*/" complete-length
(defn unsatisfied-range [opts]
  (let [complete-length (complete-length opts)]
    (p/into
     {}
     (p/sequence-group
      (p/ignore
       (p/pattern-parser #"\*\/"))
      (p/as-entry
       ::rfc/complete-length
       complete-length)))))
