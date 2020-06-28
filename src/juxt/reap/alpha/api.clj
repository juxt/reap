;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.api
  (:require
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.regex :as re]))

;; Accept

(def ^:private precompiled-accept (rfc7231/accept))

(defn accept [s]
  (when s
    (precompiled-accept (re/input s))))

(defn accept-when-string [s]
  (cond-> s (string? s) accept))

;; Accept-Charset

(def ^:private precompiled-accept-charset (rfc7231/accept-charset))

(defn accept-charset [s]
  (when s
    (precompiled-accept-charset (re/input s))))

(defn accept-charset-when-string [s]
  (cond-> s (string? s) accept-charset))

;; Accept-Language

(def ^:private precompiled-accept-language (rfc7231/accept-language))

(defn accept-language [s]
  (when s
    (precompiled-accept-language (re/input s))))

(defn accept-language-when-string [s]
  (cond-> s (string? s) accept-language))

;; Accept-Encoding

(def ^:private precompiled-accept-encoding (rfc7231/accept-encoding))

(defn accept-encoding [s]
  (when s
    (precompiled-accept-encoding (re/input s))))

(defn accept-encoding-when-string [s]
  (cond-> s (string? s) accept-encoding))

;; Content-Type

(def ^:private precompiled-content-type (rfc7231/content-type))

(defn content-type [s]
  (when s
    (precompiled-content-type (re/input s))))

(defn content-type-when-string [s]
  (cond-> s (string? s) content-type))

;; Content-Language

(def ^:private precompiled-content-language (rfc7231/content-language))

(defn content-language [s]
  (when s
    (precompiled-content-language (re/input s))))

(defn content-language-when-string [s]
  (cond-> s (string? s) content-language))

;; Content-Encoding

(def ^:private precompiled-content-encoding (rfc7231/content-encoding))

(defn content-encoding [s]
  (when s
    (precompiled-content-encoding (re/input s))))

(defn content-encoding-when-string [s]
  (cond-> s (string? s) content-encoding))
