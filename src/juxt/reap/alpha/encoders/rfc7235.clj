;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.encoders.rfc7235
  (:require
   [juxt.reap.alpha.rfc7235 :as rfc]
   [juxt.reap.alpha.decoders.rfc7230 :as rfc7230 :refer [token]]
   [clojure.string :as str]))

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

;; credentials = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param )
;;  *( OWS "," [ OWS auth-param ] ) ] ) ]
(defn credentials [opts]
  (let [auth-param (auth-param opts)]
    (fn [{::rfc/keys [auth-scheme auth-params]}]
      (str
       auth-scheme
       " "
       (str/join
        ", "
        (for [p auth-params]
          (auth-param p)))))))

;; Authorization = credentials
(def authorization credentials)
