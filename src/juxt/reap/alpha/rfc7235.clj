;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7235
  (:require
   [juxt.reap.alpha :as reap]
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7230 :as rfc7230 :refer [token OWS]]
   [juxt.reap.alpha.rfc5234 :as rfc5234 :refer [SP]]))

(set! *warn-on-reflection* true)

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; BWS = <BWS, see [RFC7230], Section 3.2.3>
(def BWS OWS)

;; quoted-string = <quoted-string, see [RFC7230], Section 3.2.6>

;; token = <token, see [RFC7230], Section 3.2.6>

;; auth-param = token BWS "=" BWS ( token / quoted-string )
(defn ^::reap/codec auth-param [_]
  {::reap/decode
   (p/into
    {}
    (p/sequence-group
     (p/as-entry
      ::auth-param-name
      (p/pattern-parser
       (re-pattern (re/re-compose "(%s)%s=%s" token BWS BWS))
       {:group 1}))
     (p/as-entry
      ::auth-param-value
      (p/alternatives
       (p/pattern-parser (re-pattern token))
       (p/comp
        rfc7230/unescape-quoted-string
        (p/pattern-parser (re-pattern rfc7230/quoted-string) {:group 1}))))))
   ::reap/encode
   (let [tok (re-pattern token)]
     (fn [{::keys [auth-param-name auth-param-value]}]
       (str
        auth-param-name
        "="
        (if (re-matches tok auth-param-value)
          auth-param-value
          (str "\"" (rfc7230/escape-quoted-string auth-param-value) "\"")))))})

;; auth-scheme = token
(def auth-scheme token)

;; token68 = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" )
;;  *"="
(def token68
  (re/re-compose "[%s]+=*" (rfc5234/merge-alternatives rfc5234/ALPHA rfc5234/DIGIT #{\- \. \_ \~ \+ \/})))

(def token68-with-lookahead
  (re/re-compose "%s(?=%s(?:,|$))" token68 OWS))

;; credentials = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param )
;;  *( OWS "," [ OWS auth-param ] ) ] ) ]
(defn ^::reap/codec credentials [opts]
  (let [auth-param (auth-param opts)]
    {::reap/decode
     (p/into
      {}
      (p/sequence-group
       (p/as-entry
        ::auth-scheme
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
            ::token68
            (p/pattern-parser
             (re-pattern token68-with-lookahead)))
           (p/as-entry
            ::auth-params
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
                  (::reap/decode auth-param))
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
                       (::reap/decode auth-param)))))))))))))))))))}))

;; Authorization = credentials
(def authorization credentials)

;; Proxy-Authorization = credentials
(def proxy-authorization credentials)

;; challenge = auth-scheme [ 1*SP ( token68 / [ ( "," / auth-param ) *(
;;  OWS "," [ OWS auth-param ] ) ] ) ]
(defn ^:juxt.reap/codec challenge [opts]
  (let [auth-param (auth-param opts)]
    {::reap/decode
     (p/into
      {}
      (p/sequence-group
       (p/as-entry
        ::auth-scheme
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
            ::token68
            (p/pattern-parser
             (re-pattern token68-with-lookahead)))
           (p/as-entry
            ::auth-params
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
                  (::reap/decode auth-param))
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
                       (::reap/decode auth-param)))))))))))))))))))}))

;; WWW-Authenticate = *( "," OWS ) challenge *( OWS "," [ OWS challenge ] )
(defn ^::reap/codec www-authenticate [opts]
  (let [challenge (challenge opts)]
    {::reap/decode
     (p/first
      (p/sequence-group
       (p/ignore
        (p/zero-or-more
         (p/pattern-parser
          (re-pattern
           (re/re-compose ",%s" OWS)))))
       (p/cons
        (::reap/decode challenge)
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
              (::reap/decode challenge))))))))))}))

(comment
  (let [p (::reap/decode (www-authenticate {}))
        m (re/input "Newauth realm=\"apps\", type=1,   title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"")]
    (p m)))

;; Proxy-Authenticate = *( "," OWS ) challenge *( OWS "," [ OWS
;;  challenge ] )

(def proxy-authenticate www-authenticate)
