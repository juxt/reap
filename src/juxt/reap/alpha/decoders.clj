;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders
  (:require
   [juxt.reap.alpha.decoders.rfc7231 :as rfc7231]
   [juxt.reap.alpha.decoders.rfc7232 :as rfc7232]
   [juxt.reap.alpha.decoders.rfc7233 :as rfc7233]
   [juxt.reap.alpha.regex :as re]))

;; Warning: This ALPHA API is very likely to change. The recommendation for now
;; is to use the functions in rfc7231 directly.

;; A set of public decoders, pre-compiled with default options, for parsing
;; common HTTP request headers.

;; Accept

(def ^:private precompiled-accept (rfc7231/accept {}))

(defn accept [s]
  (when s
    (precompiled-accept (re/input s))))

;; Accept-Charset

(def ^:private precompiled-accept-charset (rfc7231/accept-charset {}))

(defn accept-charset [s]
  (when s
    (precompiled-accept-charset (re/input s))))

;; Accept-Language

(def ^:private precompiled-accept-language (rfc7231/accept-language {}))

(defn accept-language [s]
  (when s
    (precompiled-accept-language (re/input s))))

;; Accept-Encoding

(def ^:private precompiled-accept-encoding (rfc7231/accept-encoding {}))

(defn accept-encoding [s]
  (when s
    (precompiled-accept-encoding (re/input s))))

;; Content-Type

(def ^:private precompiled-content-type (rfc7231/content-type {}))

(defn content-type [s]
  (when s
    (precompiled-content-type (re/input s))))

;; Content-Language

(def ^:private precompiled-content-language (rfc7231/content-language {}))

(defn content-language [s]
  (when s
    (precompiled-content-language (re/input s))))

;; Content-Encoding

(def ^:private precompiled-content-encoding (rfc7231/content-encoding {}))

(defn content-encoding [s]
  (when s
    (precompiled-content-encoding (re/input s))))

;; HTTP-date

(def ^:private precompiled-http-date-decoder (rfc7231/http-date {}))

(defn ^java.util.Date http-date [s]
  (when s
    (precompiled-http-date-decoder (re/input s))))

;; If-Match

(def ^:private precompiled-if-match (rfc7232/if-match {}))

(defn if-match [s]
  (when s
    (precompiled-if-match (re/input s))))

;; If-None-Match

(def ^:private precompiled-if-none-match (rfc7232/if-none-match {}))

(defn if-none-match [s]
  (when s
    (precompiled-if-none-match (re/input s))))

;; entity-tag

(def ^:private precompiled-entity-tag (rfc7232/entity-tag {}))

(defn entity-tag [s]
  (when s
    (precompiled-entity-tag (re/input s))))

;; Range

(def ^:private precompiled-range (rfc7233/range {}))

(defn range [s]
  (when s
    (precompiled-range (re/input s))))

;; If-Range

(def ^:private precompiled-if-range (rfc7233/if-range {}))

(defn if-range [s]
  (when s
    (precompiled-if-range (re/input s))))
