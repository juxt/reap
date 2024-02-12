;; Copyright © 2021, JUXT LTD.

(ns juxt.reap.decoders.rfc3986
  (:require
   [juxt.reap.combinators :as p]
   [juxt.reap.regex :as re]
   [juxt.reap.decoders.rfc5234 :as rfc5234 :refer [ALPHA DIGIT]]))

(declare reg-name)
(declare unreserved)
(declare pct-encoded)
(declare sub-delims)

;;    URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

;;    hier-part     = "//" authority path-abempty
;;                  / path-absolute
;;                  / path-rootless
;;                  / path-empty

;;    URI-reference = URI / relative-ref

;;    absolute-URI  = scheme ":" hier-part [ "?" query ]

;;    relative-ref  = relative-part [ "?" query ] [ "#" fragment ]

;;    relative-part = "//" authority path-abempty
;;                  / path-absolute
;;                  / path-noscheme
;;                  / path-empty

;;    scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

;;    authority     = [ userinfo "@" ] host [ ":" port ]
;;    userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )

;;    host          = IP-literal / IPv4address / reg-name
(defn host [_]
  (p/alternatives
   (p/pattern-parser (re-pattern reg-name))))

;;    port          = *DIGIT
(def port #"[0-9]*")

;;    IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"

;;    IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )

;;    IPv6address   =                            6( h16 ":" ) ls32
;;                  /                       "::" 5( h16 ":" ) ls32
;;                  / [               h16 ] "::" 4( h16 ":" ) ls32
;;                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
;;                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
;;                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
;;                  / [ *4( h16 ":" ) h16 ] "::"              ls32
;;                  / [ *5( h16 ":" ) h16 ] "::"              h16
;;                  / [ *6( h16 ":" ) h16 ] "::"

;;    h16           = 1*4HEXDIG
;;    ls32          = ( h16 ":" h16 ) / IPv4address
;;    IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet

;;    dec-octet     = DIGIT                 ; 0-9
;;                  / %x31-39 DIGIT         ; 10-99
;;                  / "1" 2DIGIT            ; 100-199
;;                  / "2" %x30-34 DIGIT     ; 200-249
;;                  / "25" %x30-35          ; 250-255

;;    reg-name      = *( unreserved / pct-encoded / sub-delims )
;; See below

;;    path          = path-abempty    ; begins with "/" or is empty
;;                  / path-absolute   ; begins with "/" but not "//"
;;                  / path-noscheme   ; begins with a non-colon segment
;;                  / path-rootless   ; begins with a segment
;;                  / path-empty      ; zero characters

;;    path-abempty  = *( "/" segment )
;;    path-absolute = "/" [ segment-nz *( "/" segment ) ]
;;    path-noscheme = segment-nz-nc *( "/" segment )
;;    path-rootless = segment-nz *( "/" segment )
;;    path-empty    = 0<pchar>

;;    segment       = *pchar
;;    segment-nz    = 1*pchar
;;    segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
;;                  ; non-zero-length segment without any colon ":"

;;    pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

;;    query         = *( pchar / "/" / "?" )

;;    fragment      = *( pchar / "/" / "?" )

;;    pct-encoded   = "%" HEXDIG HEXDIG
(def pct-encoded #"%[0-9A-F]{2}")

;;    unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
(def ^{:type ::rfc5234/alternatives} unreserved
  (rfc5234/merge-alternatives ALPHA DIGIT #{\- \. \_ \~}))

;;    gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
(def ^{:type ::rfc5234/alternatives} gen-delims
  (rfc5234/merge-alternatives  #{\: \/ \? \# \[ \] \@}))

;;    sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
;;                  / "*" / "+" / "," / ";" / "="
(def ^{:type ::rfc5234/alternatives} sub-delims
  (rfc5234/merge-alternatives #{\!  \$  \&  \'  \(  \) \*  \+  \,  \;  \=}))

;;    reserved      = gen-delims / sub-delims
(def ^{:type ::rfc5234/alternatives} reserved
  (rfc5234/merge-alternatives gen-delims sub-delims))

(def reg-name
  (re/re-compose "(?:%s|[%s])*" pct-encoded (rfc5234/merge-alternatives unreserved sub-delims)))
