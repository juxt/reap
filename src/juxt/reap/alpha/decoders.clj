;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders
  (:require
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.regex :as re]))

;; Warning: This ALPHA API is very likely to change. The recommendation for now
;; is to use the functions in rfc7231 directly.

;; A set of public decoders, pre-compiled with default options, for parsing
;; common HTTP request headers.

;; Accept

(def ^:private precompiled-accept (rfc7231/accept {}))

(defn accept [s]
  (when s
    ((:juxt.reap/decode precompiled-accept)
     (re/input s))))

;; Accept-Charset

(def ^:private precompiled-accept-charset (rfc7231/accept-charset {}))

(defn accept-charset [s]
  (when s
    ((:juxt.reap/decode precompiled-accept-charset)
     (re/input s))))

;; Accept-Language

(def ^:private precompiled-accept-language (rfc7231/accept-language {}))

(defn accept-language [s]
  (when s
    ((:juxt.reap/decode precompiled-accept-language)
     (re/input s))))

;; Accept-Encoding

(def ^:private precompiled-accept-encoding (rfc7231/accept-encoding {}))

(defn accept-encoding [s]
  (when s
    ((:juxt.reap/decode precompiled-accept-encoding)
     (re/input s))))

;; Content-Type

(def ^:private precompiled-content-type (rfc7231/content-type {}))

(defn content-type [s]
  (when s
    ((:juxt.reap/decode precompiled-content-type)
     (re/input s))))

;; Content-Language

(def ^:private precompiled-content-language (rfc7231/content-language {}))

(defn content-language [s]
  (when s
    ((:juxt.reap/decode precompiled-content-language)
     (re/input s))))

;; Content-Encoding

(def ^:private precompiled-content-encoding (rfc7231/content-encoding {}))

(defn content-encoding [s]
  (when s
    ((:juxt.reap/decode precompiled-content-encoding)
     (re/input s))))
