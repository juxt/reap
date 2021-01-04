;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.encoders.rfc7235
  (:require
   [juxt.reap.alpha.rfc7235 :as rfc]
   [juxt.reap.alpha.decoders.rfc7230 :as rfc7230 :refer [token]]
   [clojure.string :as str]))

(declare challenge)
(declare credentials)

;; Authorization = credentials
(defn authorization [opts]
  (credentials opts))

;; BWS = <BWS, see [RFC7230], Section 3.2.3>

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; Proxy-Authenticate = *( "," OWS ) challenge *( OWS "," [ OWS
;;  challenge ] )
(defn proxy-authenticate [opts]
  (let [challenge (challenge opts)]
    (fn [coll]
      (str/join
       ", "
       (for [c coll]
         (challenge c))))))

;; Proxy-Authorization = credentials
(defn proxy-authorization [opts]
  (credentials opts))

;; WWW-Authenticate = *( "," OWS ) challenge *( OWS "," [ OWS challenge ] )
(defn www-authenticate [opts]
  (let [challenge (challenge opts)]
    (fn [coll]
      (str/join
       ", "
       (for [c coll]
         (challenge c))))))

;; auth-param = token BWS "=" BWS ( token / quoted-string )
(defn auth-param [_]
  (let [tok (re-pattern token)]
    (fn [{::rfc/keys [auth-param-name auth-param-value]}]
      (str
       auth-param-name
       "="
       (if (re-matches tok auth-param-value)
         auth-param-value
         (str "\"" (rfc7230/escape-quoted-string auth-param-value) "\""))))))

;; auth-scheme = token

;; challenge = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param ) *(
;;  OWS "," [ OWS auth-param ] ) ] ) ]
(defn challenge [opts]
  (let [auth-param (auth-param opts)]
    (fn [{::rfc/keys [auth-scheme token68 auth-params]}]
      (cond
        token68
        (str auth-scheme " "token68)
        auth-params
        (str
         auth-scheme
         " "
         (str/join
          ", "
          (map auth-param auth-params)))))))

;; credentials = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param )
;;  *( OWS "," [ OWS auth-param ] ) ] ) ]
(defn credentials [opts]
  (let [auth-param (auth-param opts)]
    (fn [{::rfc/keys [auth-scheme token68 auth-params]}]
      (cond
        token68
        (str auth-scheme " " token68)
        auth-params
        (str
         auth-scheme
         " "
         (str/join
          ", "
          (map auth-param auth-params)))))))

;; quoted-string = <quoted-string, see [RFC7230], Section 3.2.6>

;; token = <token, see [RFC7230], Section 3.2.6>

;; token68 = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" )
;;  *"="
