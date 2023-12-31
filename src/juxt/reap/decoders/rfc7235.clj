;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.decoders.rfc7235
  (:require
   [juxt.reap.combinators :as p]
   [juxt.reap.regex :as re]
   [juxt.reap.rfc7235 :as rfc]
   [juxt.reap.decoders.rfc7230 :as rfc7230 :refer [token OWS]]
   [juxt.reap.decoders.rfc5234 :as rfc5234 :refer [SP]]))

(set! *warn-on-reflection* true)

(declare challenge)
(declare credentials)
(declare token68)
(declare token68-with-lookahead)
(declare www-authenticate)

;; Authorization = credentials
(defn authorization [opts]
  (p/complete
   (credentials opts)))

;; BWS = <BWS, see [RFC7230], Section 3.2.3>
(def BWS OWS)

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; Proxy-Authenticate = *( "," OWS ) challenge *( OWS "," [ OWS
;;  challenge ] )
(defn proxy-authenticate [opts]
  (p/complete
   (www-authenticate opts)))

;; Proxy-Authorization = credentials
(defn proxy-authorization [opts]
  (p/complete (credentials opts)))

;; WWW-Authenticate = *( "," OWS ) challenge *( OWS "," [ OWS challenge ] )
(defn www-authenticate [opts]
  (let [challenge (challenge opts)]
    (p/complete
     (p/first
      (p/sequence-group
       (p/ignore
        (p/zero-or-more
         (p/pattern-parser
          (re-pattern
           (re/re-compose ",%s" OWS)))))
       (p/cons
        challenge
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern
              (re/re-compose ",%s" OWS))))
           (p/optionally
            (p/first
             (p/sequence-group
              (p/ignore
               (p/pattern-parser
                (re-pattern OWS)))
              challenge))))))))))))

;; auth-param = token BWS "=" BWS ( token / quoted-string )
(defn auth-param [_]
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     ::rfc/auth-param-name
     (p/pattern-parser
      (re-pattern (re/re-compose "(%s)%s=%s" token BWS BWS))
      {:group 1}))
    (p/as-entry
     ::rfc/auth-param-value
     (p/alternatives
      (p/pattern-parser (re-pattern token))
      (p/comp
       rfc7230/unescape-quoted-string
       (p/pattern-parser (re-pattern rfc7230/quoted-string) {:group 1})))))))

;; auth-scheme = token
(def auth-scheme token)

;; challenge = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param ) *(
;;  OWS "," [ OWS auth-param ] ) ] ) ]
(defn challenge [opts]
  (let [auth-param (auth-param opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry
       ::rfc/auth-scheme
       (p/pattern-parser
        (re-pattern auth-scheme)))
      (p/optionally
       (p/first
        (p/sequence-group
         (p/ignore
          (p/pattern-parser
           (re-pattern
            (re/re-compose "%s" SP))))
         (p/alternatives
          (p/as-entry
           ::rfc/token68
           (p/pattern-parser
            (re-pattern token68-with-lookahead)))
          (p/as-entry
           ::rfc/auth-params
           (p/comp
            vec
            (p/optionally
             (p/first
              (p/sequence-group
               (p/cons
                (p/alternatives
                 (p/ignore
                  (p/pattern-parser
                   (re-pattern #",")))
                 auth-param)
                (p/zero-or-more
                 (p/first
                  (p/sequence-group
                   (p/ignore
                    (p/pattern-parser
                     (re-pattern
                      ;; We add a bit of negative lookahead to ensure we
                      ;; don't eagerly consume the comma that is
                      ;; separating challenges in
                      ;; www-authenticate. Doing so will prevent parsing
                      ;; of www-authenticate to continue passed the
                      ;; first challenge.
                      (re/re-compose "%s%s(?!%s%s%s%s)" OWS "," OWS token SP token))))
                   (p/optionally
                    (p/first
                     (p/sequence-group
                      (p/ignore (p/pattern-parser (re-pattern OWS)))
                      auth-param))))))))))))))))))))

;; credentials = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param )
;;  *( OWS "," [ OWS auth-param ] ) ] ) ]
(defn credentials [opts]
  (let [auth-param (auth-param opts)]
    (p/into
     {}
     (p/sequence-group
      (p/as-entry
       ::rfc/auth-scheme
       (p/pattern-parser
        (re-pattern auth-scheme)))
      (p/optionally
       (p/first
        (p/sequence-group
         (p/ignore
          (p/pattern-parser
           (re-pattern
            (re/re-compose "%s" SP))))
         (p/alternatives
          (p/as-entry
           ::rfc/token68
           (p/pattern-parser
            (re-pattern token68-with-lookahead)))
          (p/as-entry
           ::rfc/auth-params
           (p/comp
            vec
            (p/optionally
             (p/first
              (p/sequence-group
               (p/cons
                (p/alternatives
                 (p/ignore
                  (p/pattern-parser
                   (re-pattern #",")))
                 auth-param)
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
                      (p/ignore (p/pattern-parser (re-pattern OWS)))
                      auth-param))))))))))))))))))))

;; quoted-string = <quoted-string, see [RFC7230], Section 3.2.6>

;; token = <token, see [RFC7230], Section 3.2.6>

;; token68 = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" )
;;  *"="
(def token68
  (re/re-compose "[%s]+=*" (rfc5234/merge-alternatives rfc5234/ALPHA rfc5234/DIGIT #{\- \. \_ \~ \+ \/})))

(def token68-with-lookahead
  (re/re-compose "%s(?=%s(?:,|$))" token68 OWS))
