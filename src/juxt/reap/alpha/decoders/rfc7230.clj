;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.decoders.rfc7230
  (:require
   [clojure.string :as string]
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.interval :as i]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.decoders.rfc5234 :as rfc5234 :refer [HTAB SP VCHAR]]
   [juxt.reap.alpha.decoders.rfc3986 :as rfc3986]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(declare OWS)
(declare port)
(declare uri-host)

;; RFC 7230

;; Appendix B.  Collected ABNF

;; BWS = OWS
(def ^String BWS OWS)

;; Connection = *( "," OWS ) connection-option *( OWS "," [ OWS
;;  connection-option ] )

;; Content-Length = 1*DIGIT

;; HTTP-message = start-line *( header-field CRLF ) CRLF [ message-body
;;  ]
;; HTTP-name = %x48.54.54.50 ; HTTP
;; HTTP-version = HTTP-name "/" DIGIT "." DIGIT
;; Host = uri-host [ ":" port ]
(defn host [opts]
  (let [uri-host (uri-host opts)]
    (p/complete
     (p/into
      {}
      (p/sequence-group
       (p/as-entry
        :host
        uri-host)
       (p/optionally
        (p/as-entry
         :port
         (p/comp
          #(when-not (str/blank? %)
             (try
               (Integer/parseInt %)
               (catch NumberFormatException e nil)))
          (p/first
           (p/sequence-group
            (p/ignore
             (p/pattern-parser #":"))
            (p/pattern-parser port)))))))))))

;; OWS = *( SP / HTAB )

(def ^String OWS (rfc5234/zero-or-more (rfc5234/alternatives SP HTAB)))

;; RWS = 1*( SP / HTAB )
(def ^String RWS (rfc5234/one-or-more (rfc5234/alternatives SP HTAB)))

;; TE = [ ( "," / t-codings ) *( OWS "," [ OWS t-codings ] ) ]
;; Trailer = *( "," OWS ) field-name *( OWS "," [ OWS field-name ] )
;; Transfer-Encoding = *( "," OWS ) transfer-coding *( OWS "," [ OWS
;;  transfer-coding ] )

;; URI-reference = <URI-reference, see [RFC3986], Section 4.1>
;; Upgrade = *( "," OWS ) protocol *( OWS "," [ OWS protocol ] )

;; Via = *( "," OWS ) ( received-protocol RWS received-by [ RWS comment
;;  ] ) *( OWS "," [ OWS ( received-protocol RWS received-by [ RWS
;;  comment ] ) ] )

;; absolute-URI = <absolute-URI, see [RFC3986], Section 4.3>
;; absolute-form = absolute-URI
;; absolute-path = 1*( "/" segment )
;; asterisk-form = "*"
;; authority = <authority, see [RFC3986], Section 3.2>
;; authority-form = authority

;; chunk = chunk-size [ chunk-ext ] CRLF chunk-data CRLF
;; chunk-data = 1*OCTET
;; chunk-ext = *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
;; chunk-ext-name = token
;; chunk-ext-val = token / quoted-string
;; chunk-size = 1*HEXDIG
;; chunked-body = *chunk last-chunk trailer-part CRLF

;; connection-option = token


;; field-content = field-vchar [ 1*( SP / HTAB ) field-vchar ]

;; field-value = *( field-content / obs-fold )
;; field-vchar = VCHAR / obs-text
;; fragment = <fragment, see [RFC3986], Section 3.5>

;; header-field = field-name ":" OWS field-value OWS
;; http-URI = "http://" authority path-abempty [ "?" query ] [ "#"
;;  fragment ]
;; https-URI = "https://" authority path-abempty [ "?" query ] [ "#"
;;  fragment ]

;; last-chunk = 1*"0" [ chunk-ext ] CRLF

;; message-body = *OCTET
;; method = token

;; obs-fold = CRLF 1*( SP / HTAB )
;; obs-text = %x80-FF
(def obs-text (i/->interval [0x80 0xFF]))

;; origin-form = absolute-path [ "?" query ]

;; partial-URI = relative-part [ "?" query ]
;; path-abempty = <path-abempty, see [RFC3986], Section 3.3>

;; port = <port, see [RFC3986], Section 3.2.3>
(def port rfc3986/port)

;; protocol = protocol-name [ "/" protocol-version ]
;; protocol-name = token
;; protocol-version = token
;; pseudonym = token

;; qdtext = HTAB / SP / "!" / %x23-5B ; '#'-'['
;;  / %x5D-7E ; ']'-'~'
;;  / obs-text
(def qdtext
  (rfc5234/alternatives
   HTAB SP \! (i/->interval [0x23 0x5B]) ; '#'-'['
   (i/->interval [0x5D 0x7E])            ; ']'-'~'
   obs-text))

;; query = <query, see [RFC3986], Section 3.4>

;; quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
(def quoted-pair
  (re/re-compose
   "\\\\[%s]"
   (apply re/re-concat (rfc5234/alternatives HTAB SP VCHAR (i/->interval [0x80 0xFF])))))

;; quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
(def quoted-string ; exposes group
  (re/re-compose "%s((?:%s|%s)*)%s" rfc5234/DQUOTE (rfc5234/optional qdtext) quoted-pair rfc5234/DQUOTE))

(defn unescape-quoted-string
  "Take a quoted-string and replace escaped characters."
  [s]
  (string/replace s (re-pattern quoted-pair) #(subs % 1)))

(defn escape-quoted-string
  "Take a string and escape DQUOTEs."
  [s]
  (string/replace s "\"" "\\\""))

;; ctext = HTAB / SP / %x21-27 ; '!'-'''
;;  / %x2A-5B ; '*'-'['
;;  / %x5D-7E ; ']'-'~'
;;  / obs-text

(def ^{:type ::rfc5234/alternatives}
  ctext
  (rfc5234/merge-alternatives
   #{rfc5234/HTAB rfc5234/SP}
   (i/->interval [0x21 0x27])
   (i/->interval [0x2A 0x5B])
   (i/->interval [0x5D 0x7E])
   obs-text))

;; comment = "(" *( ctext / quoted-pair / comment ) ")"

(defn rfc-comment []
  (p/sequence-group
   [(p/pattern-parser (re-pattern "\\("))
    (p/zero-or-more
     (p/alternatives
      (p/pattern-parser (re-pattern (re/re-compose "[%s]" ctext)))
      #_(p/pattern-parser (re-pattern quoted-pair))
      #_(fn [matcher]
          ((rfc-comment) matcher))))]
   (p/pattern-parser (re-pattern "\\)"))))

;; TODO: Why doesn't this take more than 2 chars?
(comment
  ((rfc-comment) (re/input "(Version)")))


;; rank = ( "0" [ "." *3DIGIT ] ) / ( "1" [ "." *3"0" ] )
;; reason-phrase = *( HTAB / SP / VCHAR / obs-text )
;; received-by = ( uri-host [ ":" port ] ) / pseudonym
;; received-protocol = [ protocol-name "/" ] protocol-version
;; relative-part = <relative-part, see [RFC3986], Section 4.2>
;; request-line = method SP request-target SP HTTP-version CRLF
;; request-target = origin-form / absolute-form / authority-form /
;;  asterisk-form

;; scheme = <scheme, see [RFC3986], Section 3.1>
;; segment = <segment, see [RFC3986], Section 3.3>
;; start-line = request-line / status-line
;; status-code = 3DIGIT
;; status-line = HTTP-version SP status-code SP reason-phrase CRLF

;; t-codings = "trailers" / ( transfer-coding [ t-ranking ] )
;; t-ranking = OWS ";" OWS "q=" rank

;; tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
;;  "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
(def ^{:type ::rfc5234/alternatives}
  tchar
  (rfc5234/merge-alternatives rfc5234/ALPHA rfc5234/DIGIT #{\! \# \$ \% \& \' \* \+ \- \. \^ \_ \` \| \~}))

;; token = 1*tchar
(def ^{:tag String
       :ref "RFC7230 Section 3.2.6"}
  token (rfc5234/one-or-more tchar))

;; trailer-part = *( header-field CRLF )
;; transfer-coding = "chunked" / "compress" / "deflate" / "gzip" /
;;  transfer-extension
;; transfer-extension = token *( OWS ";" OWS transfer-parameter )
;; transfer-parameter = token BWS "=" BWS ( token / quoted-string )

;; uri-host = <host, see [RFC3986], Section 3.2.2>
(def uri-host rfc3986/host)

;; field-name = token
(def field-name token)
