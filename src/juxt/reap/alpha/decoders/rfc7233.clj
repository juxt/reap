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
   (p/array-map ::rfc/byte-content-range (byte-content-range opts))
   (p/array-map ::rfc/other-content-range (other-content-range opts))))

(comment
  ((content-range {}) (re/input "bytes 134387-222839/2723780")))

(comment
  ((content-range {}) (re/input "pages 1-2/20")))

;; HTTP-date = <HTTP-date, see [RFC7231], Section 7.1.1.1>

;; If-Range = entity-tag / HTTP-date
(defn if-range [opts]
  (p/array-map
   ::rfc/if-range
   (p/alternatives
    (p/array-map ::rfc7232/entity-tag (entity-tag opts))
    (p/array-map ::rfc7231/http-date (http-date opts)))))

(comment
  ((if-range {}) (re/input "W/\"foo\""))
  ((if-range {}) (re/input "Tue, 29 Dec 2020 00:41:56 GMT")))

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; Range = byte-ranges-specifier / other-ranges-specifier
(defn range [_]
  (p/alternatives
   (p/array-map ::rfc/byte-ranges-specifier byte-ranges-specifier)
   (p/array-map ::rfc/other-ranges-specifier other-ranges-specifier)))

(comment
  ((range {}) (re/input "bytes=10-20,  30-40, 80-   ")))

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

(comment
  ((acceptable-ranges {}) (re/input "foo,zip,  bar")))

;; byte-content-range = bytes-unit SP ( byte-range-resp / unsatisfied-range )
(defn byte-content-range [_]
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     ::rfc/bytes-unit
     bytes-unit)
    (p/ignore
     (p/pattern-parser
      (re-pattern
       (re/re-compose "%s" SP))))
    (p/alternatives
     (p/as-entry ::rfc/byte-range-resp byte-range-resp)
     (p/as-entry ::rfc/unsatisfied-range unsatisfied-range)))))

(comment
  (byte-content-range (re/input "bytes 10-20/30")))

(comment
  (byte-content-range (re/input "bytes */100")))

;; byte-range = first-byte-pos "-" last-byte-pos
(def byte-range
  (p/into
   {}
   (p/sequence-group
    (p/as-entry ::rfc/first-byte-pos last-byte-pos)
    (p/ignore (p/pattern-parser #"\-"))
    (p/as-entry ::rfc/last-byte-pos last-byte-pos))))

;; byte-range-resp = byte-range "/" ( complete-length / "*" )
(def byte-range-resp
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
     (p/pattern-parser #"\*")))))

(comment
  (byte-range-resp (re/input "123-32/98980")))

;; byte-range-set = *( "," OWS ) ( byte-range-spec /
;;  suffix-byte-range-spec ) *( OWS "," [ OWS ( byte-range-spec /
;;  suffix-byte-range-spec ) ] )
(def byte-range-set
  (p/first
   (p/sequence-group
    (p/ignore
     (p/pattern-parser
      (re-pattern
       (re/re-compose "(?:%s%s)*" "," OWS))))
    (p/cons
     (p/alternatives
      (p/array-map
       ::rfc/byte-range-spec
       byte-range-spec)
      (p/array-map
       ::rfc/suffix-byte-range-spec
       suffix-byte-range-spec))
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
            (p/array-map
             ::rfc/byte-range-spec
             byte-range-spec)
            (p/array-map
             ::rfc/suffix-byte-range-spec
             suffix-byte-range-spec))))))))))))

(comment
  (byte-range-set (re/input ",,,  ,  20-30 , 50-80,90-\t , -10")))

;; byte-range-spec = first-byte-pos "-" [ last-byte-pos ]
(def byte-range-spec
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
      last-byte-pos)))))

(comment
  (byte-range-spec (re/input "12-90")))

;; byte-ranges-specifier = bytes-unit "=" byte-range-set
(def byte-ranges-specifier
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     ::rfc/bytes-unit
     bytes-unit)
    (p/ignore
     (p/pattern-parser #"\="))
    (p/optionally
     (p/as-entry
      ::rfc/byte-range-set
      byte-range-set)))))

(comment
  (byte-ranges-specifier (re/input "bytes=10-20")))

;; bytes-unit = "bytes"
(def bytes-unit (p/pattern-parser (re-pattern "bytes")))

;; complete-length = 1*DIGIT
(def complete-length
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; entity-tag = <entity-tag, see [RFC7232], Section 2.3>

;; first-byte-pos = 1*DIGIT
(def first-byte-pos
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; last-byte-pos = 1*DIGIT
(def last-byte-pos
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; other-content-range = other-range-unit SP other-range-resp
(defn other-content-range [_]
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     ::rfc/other-range-unit
     other-range-unit)
    (p/ignore
     (p/pattern-parser
      (re-pattern
       (re/re-compose "%s" SP))))
    (p/as-entry
     ::rfc/other-range-resp
     (p/pattern-parser (re-pattern other-range-resp))))))

(comment
  (other-content-range (re/input "pages 1-2,4-5,10-")))

;; other-range-resp = *CHAR
(def ^String other-range-resp (re/re-compose "%s*" CHAR))

;; other-range-set = 1*VCHAR
(def ^String other-range-set (re/re-compose "%s+" VCHAR))

;; other-range-unit = token
(def other-range-unit (p/pattern-parser (re-pattern token)))

;; other-ranges-specifier = other-range-unit "=" other-range-set
(def other-ranges-specifier
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     ::rfc/other-range-unit
     other-range-unit)
    (p/ignore
     (p/pattern-parser #"\="))
    (p/optionally
     (p/as-entry
      ::rfc/other-range-set
      (p/pattern-parser (re-pattern other-range-set)))))))

(comment
  (other-ranges-specifier (re/input "foo=abc")))

;; range-unit = bytes-unit / other-range-unit
(defn range-unit [_]
  (p/alternatives
   bytes-unit
   other-range-unit))

;; suffix-byte-range-spec = "-" suffix-length
(def suffix-byte-range-spec
  (p/into
   {}
   (p/sequence-group
    (p/ignore
     (p/pattern-parser #"\-"))
    (p/as-entry
     ::rfc/suffix-length
     suffix-length))))

;; suffix-length = 1*DIGIT
(def suffix-length
  (p/comp
   #(Long/parseLong %)
   (p/pattern-parser
    (re-pattern
     (re/re-compose "%s+" DIGIT)))))

;; token = <token, see [RFC7230], Section 3.2.6>

;; unsatisfied-range = "*/" complete-length
(def unsatisfied-range
  (p/into
   {}
   (p/sequence-group
    (p/ignore
     (p/pattern-parser #"\*\/"))
    (p/as-entry
     ::rfc/complete-length
     complete-length))))

(comment
  (unsatisfied-range (re/input "*/10")))
