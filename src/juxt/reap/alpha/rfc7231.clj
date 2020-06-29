;; Copyright © 2020, JUXT LTD.

;; TODO: Rename juxt.reap.alpha to juxt.http

(ns juxt.reap.alpha.rfc7231
  (:refer-clojure :exclude [type])
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.rfc4647 :as rfc4647]
   [juxt.reap.alpha.rfc5234 :as rfc5234 :refer [DIGIT]]
   [juxt.reap.alpha.rfc5646 :as rfc5646]
   [juxt.reap.alpha.rfc7230 :as rfc7230 :refer [OWS RWS token]]

   [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; parameter = token "=" ( token / quoted-string )

(defn ^:juxt.reap/codec parameter
  "Return a parameter parser that parses into map containing :name
  and :value keys."
  [_]
  {:juxt.reap/decode
   (p/into
    {}
    (p/sequence-group
     (p/as-entry
      :juxt.reap.alpha/parameter-name
      (p/pattern-parser
       (re-pattern token)))
     (p/first
      (p/sequence-group
       (p/ignore (p/pattern-parser #"="))
       (p/as-entry
        :juxt.reap.alpha/parameter-value
        (p/alternatives
         (p/pattern-parser (re-pattern token))
         (p/comp
          rfc7230/unescape-quoted-string
          (p/pattern-parser
           (re-pattern rfc7230/quoted-string) {:group 1}))))))))})

(defn ^:juxt.reap/codec optional-parameter
  "Return a parameter parser that parses into map containing :name and,
  optionally, a :value key."
  [_]
  {:juxt.reap/decode
   (p/into
    {}
    (p/sequence-group
     (p/as-entry
      :juxt.reap.alpha/parameter-name
      (p/pattern-parser
       (re-pattern token)))
     (p/optionally
      (p/first
       (p/sequence-group
        (p/ignore (p/pattern-parser #"="))
        (p/as-entry
         :juxt.reap.alpha/parameter-value
         (p/alternatives
          (p/pattern-parser (re-pattern token))
          (p/comp
           rfc7230/unescape-quoted-string
           (p/pattern-parser
            (re-pattern rfc7230/quoted-string) {:group 1})))))))))})

;; token = <token, see [RFC7230], Section 3.2.6>

;; subtype = token
(def ^String subtype token)

;; type = token
(def ^String type token)



;; Allow = [ ( "," / method ) *( OWS "," [ OWS method ] ) ]

;; BWS = <BWS, see [RFC7230], Section 3.2.3>

;; language-tag = <Language-Tag, see [RFC5646], Section 2.1>
(def language-tag rfc5646/language-tag)

;; Content-Language = *( "," OWS ) language-tag *( OWS "," [ OWS
;;  language-tag ] )

(defn ^:juxt.reap/codec content-language [opts]
  (let [language-tag (language-tag opts)]
    {:juxt.reap/decode
     (p/first
      (p/sequence-group
       (p/ignore
        (p/zero-or-more
         (p/sequence-group
          (p/pattern-parser (re-pattern ","))
          (p/pattern-parser (re-pattern OWS)))))
       (p/cons
        (:juxt.reap/decode language-tag)
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser (re-pattern OWS)))
           (p/ignore
            (p/pattern-parser (re-pattern ",")))
           (p/optionally
            (p/first
             (p/sequence-group
              (p/ignore
               (p/pattern-parser (re-pattern OWS)))
              (:juxt.reap/decode language-tag))))))))))}))

;; Content-Location = absolute-URI / partial-URI

;; Date = HTTP-date

;; Expect = "100-continue"

;; From = mailbox

;; GMT = %x47.4D.54 ; GMT

;; HTTP-date = IMF-fixdate / obs-date

;; IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT

;; Location = URI-reference

;; Max-Forwards = 1*DIGIT

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; RWS = <RWS, see [RFC7230], Section 3.2.3>
;; Referer = absolute-URI / partial-URI
;; Retry-After = HTTP-date / delay-seconds

;; URI-reference = <URI-reference, see [RFC7230], Section 2.7>
;; User-Agent = product *( RWS ( product / comment ) )

;; Vary = "*" / ( *( "," OWS ) field-name *( OWS "," [ OWS field-name ] ) )
(defn vary []
  {:juxt.reap/decode
   (p/alternatives
    (p/array-map
     :juxt.reap.alpha/wildcard
     (p/pattern-parser #"\*"))
    (p/cons
     (p/first
      (p/sequence-group
       (p/ignore
        (p/zero-or-more
         (p/sequence-group
          (p/pattern-parser (re-pattern ","))
          (p/pattern-parser (re-pattern OWS)))))
       (p/array-map
        :juxt.reap.alpha/field-name
        (p/pattern-parser
         (re-pattern rfc7230/field-name)))))
     (p/zero-or-more
      (p/first
       (p/sequence-group
        (p/ignore (p/pattern-parser (re-pattern (str OWS ","))))
        (p/optionally
         (p/first
          (p/sequence-group
           (p/ignore (p/pattern-parser (re-pattern OWS)))
           (p/array-map
            :juxt.reap.alpha/field-name
            (p/pattern-parser (re-pattern rfc7230/field-name)))))))))))
   :juxt.reap/encode
   (fn vary-str [coll]
     (str/join
      ","
      (for [i coll]
        (cond
          (:juxt.reap.alpha/wildcard i) "*"
          :else (:juxt.reap.alpha/field-name i)))))})

;; TODO: How to build from (#:juxt.reap.alpha{:field-name "accept"} #:juxt.reap.alpha{:field-name "foo"}) to accept,accept-charset

;; absolute-URI = <absolute-URI, see [RFC7230], Section 2.7>

;; accept-ext = OWS ";" OWS token [ "=" ( token / quoted-string ) ]
(defn ^:juxt.reap/codec accept-ext [opts]
  {:juxt.reap/decode
   (let [optional-parameter (optional-parameter opts)]
     (p/first
      (p/sequence-group
       (p/ignore
        (p/pattern-parser
         (re-pattern
          (re/re-concat OWS \; OWS))))
       (:juxt.reap/decode optional-parameter))))})

(comment
  ((p/zero-or-more (:juxt.reap/decode (accept-ext {}))) (re/input ";a;b;c=d")))

;; qvalue = ( "0" [ "." *3DIGIT ] ) / ( "1" [ "." *3"0" ] )
(def qvalue (re/re-compose "0(?:\\.[%s]{0,3})?|1(?:\\.[0]{0,3})?" DIGIT))

;; weight = OWS ";" OWS "q=" qvalue
(defn ^:juxt.reap/codec weight [_]
  {:juxt.reap/decode
   (p/comp
    #(Double/parseDouble %)
    (p/pattern-parser
     (re-pattern
      (re/re-concat
       OWS \; OWS "(?i:q=" (re/group qvalue) ")"))
     {:group 1}))})

(comment
  ((:juxt.reap/decode (weight {})) (re/input ";q=0.8")))

;; accept-params = weight *accept-ext
(defn ^:juxt.reap/codec accept-params [opts]
  {:juxt.reap/decode
   (let [weight (weight opts)
         accept-ext (accept-ext opts)]
     (p/into
      {}
      (p/sequence-group
       (p/as-entry
        :juxt.reap.alpha/qvalue
        (:juxt.reap/decode weight))
       (p/as-entry
        :juxt.reap.alpha/accept-ext
        (p/comp
         vec
         (p/seq ; ignore if empty list
          (p/zero-or-more
           (:juxt.reap/decode accept-ext))))))))})

(comment
  ;; This will return nil, since accept-params must start with a qvalue.
  ((:juxt.reap/decode (accept-params {})) (re/input ";foo=bar")))

(comment
  ((:juxt.reap/decode (accept-params {})) (re/input ";q=0.8")))

(comment
  ((:juxt.reap/decode (accept-params {})) (re/input ";q=0.8;a;b;c=d")))


;; asctime-date = day-name SP date3 SP time-of-day SP year

;; charset = token
(def ^String charset token)

;; content-coding = token
(def ^String content-coding token)

;; Content-Encoding = *( "," OWS ) content-coding *( OWS "," [ OWS
;;  content-coding ] )

(defn ^:juxt.reap/codec content-encoding [_]
  {:juxt.reap/decode
   (p/first
    (p/sequence-group
     (p/ignore
      (p/zero-or-more
       (p/sequence-group
        (p/pattern-parser (re-pattern ","))
        (p/pattern-parser (re-pattern OWS)))))
     (p/cons
      (p/array-map :juxt.reap.alpha/content-coding (p/pattern-parser (re-pattern content-coding)))
      (p/zero-or-more
       (p/first
        (p/sequence-group
         (p/ignore
          (p/pattern-parser (re-pattern OWS)))
         (p/ignore
          (p/pattern-parser (re-pattern ",")))
         (p/first
          (p/optionally
           (p/sequence-group
            (p/ignore (p/pattern-parser (re-pattern OWS)))
            (p/array-map :juxt.reap.alpha/content-coding (p/pattern-parser (re-pattern content-coding))))))))))))})

(comment
  ((:juxt.reap/decode (content-encoding {}))
   (re/input ",,,, , , foo,zip,qux")))

;; codings = content-coding / "identity" / "*"
(defn ^:juxt.reap/codec codings [_]
  {:juxt.reap/decode
   (p/alternatives
    (p/pattern-parser (re-pattern content-coding))
    (p/pattern-parser (re-pattern "identity"))
    (p/pattern-parser (re-pattern "\\*")))})

;; comment = <comment, see [RFC7230], Section 3.2.6>

;; date1 = day SP month SP year
;; date2 = day "-" month "-" 2DIGIT
;; date3 = month SP ( 2DIGIT / ( SP DIGIT ) )
;; day = 2DIGIT
;; day-name = %x4D.6F.6E ; Mon
;;  / %x54.75.65 ; Tue
;;  / %x57.65.64 ; Wed
;;  / %x54.68.75 ; Thu
;;  / %x46.72.69 ; Fri
;;  / %x53.61.74 ; Sat
;;  / %x53.75.6E ; Sun
;; day-name-l = %x4D.6F.6E.64.61.79 ; Monday
;;  / %x54.75.65.73.64.61.79 ; Tuesday
;;  / %x57.65.64.6E.65.73.64.61.79 ; Wednesday
;;  / %x54.68.75.72.73.64.61.79 ; Thursday
;;  / %x46.72.69.64.61.79 ; Friday
;;  / %x53.61.74.75.72.64.61.79 ; Saturday
;;  / %x53.75.6E.64.61.79 ; Sunday
;; delay-seconds = 1*DIGIT

;; field-name    = <field-name, see [RFC7230], Section 3.2>
;; As per verified errata (https://www.rfc-editor.org/errata_search.php?rfc=7231)

;; hour = 2DIGIT

;; language-range = <language-range, see [RFC4647], Section 2.1>

;; mailbox = <mailbox, see [RFC5322], Section 3.4>

;; quoted-string = <quoted-string, see [RFC7230], Section 3.2.6>


;;(identity rfc7230/quoted-string)

;; media-range = ( "*/*" / ( type "/*" ) / ( type "/" subtype ) ) *( OWS
;;  ";" OWS parameter )

(defn media-range-without-parameters [_]
  (p/alternatives
   (p/comp
    (fn [_]
      #:juxt.reap.alpha
      {:media-range "*/*"
       :type "*"
       :subtype "*"})
    (p/pattern-parser #"\*/\*"))
   (p/comp
    (fn [[media-type type]]
      #:juxt.reap.alpha
      {:media-range media-type
       :type type
       :subtype "*"})
    (p/pattern-parser
     (re-pattern (re/re-compose "(%s)/\\*" type))))
   (p/comp
    (fn [[media-type type subtype]]
      #:juxt.reap.alpha
      {:media-range media-type
       :type type
       :subtype subtype})
    (p/pattern-parser
     (re-pattern (re/re-compose "(%s)/(%s)" type subtype))))))

(defn parameters-map [parser]
  (fn [matcher]
    (when-let [parameters (parser matcher)]
      {:juxt.reap.alpha/parameters parameters
       :juxt.reap.alpha/parameter-map
       (into
        {}
        (map
         (juxt
          (comp
           ;; We lower-case to support case-insensitive lookups
           str/lower-case
           :juxt.reap.alpha/parameter-name)
          :juxt.reap.alpha/parameter-value)
         parameters))})))

(defn ^:juxt.reap/codec media-range [opts]
  {:juxt.reap/decode
   (let [parameter (parameter opts)]
     (p/comp
      #(apply merge %)
      (p/sequence-group
       (media-range-without-parameters opts)
       (parameters-map
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern (re/re-concat OWS \; OWS))))
           (:juxt.reap/decode parameter))))))))})

(comment
  ((:juxt.reap/decode (media-range {}))
   (re/input "text/html ;  foo=bar ;  baz=\"qux;quuz\" ; q=0.9; a=b ; c")))

;; media-type = type "/" subtype *( OWS ";" OWS parameter )
(defn ^:juxt.reap/codec media-type [opts]
  {:juxt.reap/decode
   (let [parameter (parameter opts)]
     (p/into
      {}
      (p/sequence-group
       (p/as-entry
        :juxt.reap.alpha/type
        (p/pattern-parser (re-pattern type)))
       (p/ignore (p/pattern-parser (re-pattern "/")))
       (p/as-entry
        :juxt.reap.alpha/subtype
        (p/pattern-parser (re-pattern subtype)))
       (parameters-map
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore (p/pattern-parser (re-pattern OWS)))
           (p/ignore (p/pattern-parser (re-pattern ";")))
           (p/ignore (p/pattern-parser (re-pattern OWS)))
           (:juxt.reap/decode parameter))))))))})

;; Content-Type = media-type
(def content-type media-type)

;; method = token
(def method token)

;; minute = 2DIGIT
;; month = %x4A.61.6E ; Jan
;;  / %x46.65.62 ; Feb
;;  / %x4D.61.72 ; Mar
;;  / %x41.70.72 ; Apr
;;  / %x4D.61.79 ; May
;;  / %x4A.75.6E ; Jun
;;  / %x4A.75.6C ; Jul
;;  / %x41.75.67 ; Aug
;;  / %x53.65.70 ; Sep
;;  / %x4F.63.74 ; Oct
;;  / %x4E.6F.76 ; Nov
;;  / %x44.65.63 ; Dec

;; obs-date = rfc850-date / asctime-date

;; partial-URI = <partial-URI, see [RFC7230], Section 2.7>

;; product-version = token

(def product-version token)

;; product = token [ "/" product-version ]

(defn ^:juxt.reap/codec product [_]
  {:juxt.reap/decode
   (p/into
    {}
    (p/sequence-group
     (p/as-entry
      :juxt.reap.alpha/product
      (p/pattern-parser (re-pattern token)))
     (p/optionally
      (p/first
       (p/sequence-group
        (p/ignore
         (p/pattern-parser (re-pattern "/")))
        (p/as-entry
         :juxt.reap.alpha/version
         (p/pattern-parser (re-pattern product-version))))))))})

;; Server = product *( RWS ( product / comment ) )

(defn ^:juxt.reap/codec server [opts]
  {:juxt.reap/decode
   (let [product (product opts)]
     (p/cons
      (:juxt.reap/decode product)
      (p/zero-or-more
       (p/first
        (p/sequence-group
         (p/ignore (p/pattern-parser (re-pattern RWS)))
         (p/alternatives
          (:juxt.reap/decode product)
          #_(rfc7230/rfc-comment)))))))})

(comment
  ((:juxt.reap/decode (server {}))
   (re/input "foo/1.2 ale1/1.0")))

;; rfc850-date = day-name-l "," SP date2 SP time-of-day SP GMT

;; second = 2DIGIT

;; time-of-day = hour ":" minute ":" second


;; Accept = [ ( "," / ( media-range [ accept-params ] ) ) *( OWS "," [
;;  OWS ( media-range [ accept-params ] ) ] ) ]
(defn ^:juxt.reap/codec accept [opts]
  {:juxt.reap/decode
   (let [parameter
         (parameter opts)

         media-range-parameter
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern (re/re-concat OWS \; OWS))))
           (:juxt.reap/decode parameter)))

         accept-params (accept-params opts)

         ;; The reason why we can't just use `media-range` is that we
         ;; need to resolve the ambiguity whereby the "q" parameter
         ;; separates media type parameters from Accept extension
         ;; parameters. This is more fully discussed in RFC 7231
         ;; Section 5.3.2.
         ;;
         ;; The trick is to create a parameter parser modelled on
         ;; `zero-or-more` which attempts to match `accept-params` for
         ;; each parameter. Since `accept-params` matches on a leading
         ;; `weight`, a weight parameter will be detected and cause the
         ;; loop to end.
         parameters-weight-accept-params
         (fn [matcher]
           (loop [matcher matcher
                  result {:juxt.reap.alpha/parameters {}}]
             (if-let [accept-params ((:juxt.reap/decode accept-params) matcher)]
               (merge result accept-params)
               (if-let [match (media-range-parameter matcher)]
                 (recur matcher
                        (update
                         result
                         :juxt.reap.alpha/parameters
                         conj [(:juxt.reap.alpha/parameter-name match)
                               (:juxt.reap.alpha/parameter-value match)]))
                 result))))]
     (p/optionally
      (p/cons
       (p/alternatives
        (p/ignore
         (p/pattern-parser #","))
        (p/comp
         #(apply merge %)
         (p/sequence-group
          (media-range-without-parameters opts)
          parameters-weight-accept-params)))
       (p/zero-or-more
        (p/first
         (p/sequence-group
          (p/ignore
           (p/pattern-parser
            (re-pattern
             (re/re-concat OWS ","))))
          (p/optionally
           (p/first
            (p/sequence-group
             (p/ignore
              (p/pattern-parser
               (re-pattern OWS)))
             (p/comp
              #(apply merge %)
              (p/sequence-group
               (media-range-without-parameters opts)
               parameters-weight-accept-params)))))))))))})

(comment
  ((:juxt.reap/decode (accept {})) (re/input "text/html;foo=bar;i=j ; q=0.8;a , application/json;v=10")))

(defn year [_]
  (p/pattern-parser
   (re-pattern (re/re-compose "%s{4}" DIGIT))))

;; year = 4DIGIT

;; Accept-Charset = *( "," OWS ) ( ( charset / "*" ) [ weight ] ) *( OWS
;;  "," [ OWS ( ( charset / "*" ) [ weight ] ) ] )

(defn ^:juxt.reap/codec accept-charset [opts]
  (let [weight (weight opts)]
    {:juxt.reap/decode
     (let [charset-with-weight
           (p/into
            {}
            (p/sequence-group
             (p/alternatives
              (p/as-entry
               :juxt.reap.alpha/charset
               (p/pattern-parser
                (re-pattern charset)))
              (p/pattern-parser
               (re-pattern (re/re-str \*))))
             (p/optionally
              (p/as-entry :juxt.reap.alpha/qvalue (:juxt.reap/decode weight)))))]
       (p/cons
        (p/first
         (p/sequence-group
          (p/ignore
           (p/pattern-parser
            (re-pattern (re/re-compose "(?:%s)*" (re/re-concat \, OWS)))))
          charset-with-weight))
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern (re/re-compose "%s%s" OWS ","))))
           (p/optionally
            (p/first
             (p/sequence-group
              (p/ignore
               (p/pattern-parser
                (re-pattern OWS)))
              charset-with-weight))))))))}))

;; Accept-Language = *( "," OWS ) ( language-range [ weight ] ) *( OWS
;;  "," [ OWS ( language-range [ weight ] ) ] )
(defn ^:juxt.reap/codec accept-language [opts]
  (let [weight (weight opts)]
    {:juxt.reap/decode
     (p/first
      (p/sequence-group
       (p/ignore
        (p/zero-or-more
         (p/pattern-parser
          (re-pattern
           (re/re-concat "," OWS)))))
       (p/cons
        (p/into
         {}
         (p/sequence-group
          (p/as-entry
           :juxt.reap.alpha/language-range
           (rfc4647/language-range opts))
          (p/optionally
           (p/as-entry
            :juxt.reap.alpha/qvalue
            (:juxt.reap/decode weight)))))
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern
              (re/re-concat OWS ","))))
           (p/optionally
            (p/into
             {}
             (p/sequence-group
              (p/ignore
               (p/pattern-parser
                (re-pattern OWS)))
              (p/as-entry
               :juxt.reap.alpha/language-range
               (rfc4647/language-range opts))
              (p/optionally
               (p/as-entry :juxt.reap.alpha/qvalue (:juxt.reap/decode weight))))))))))))}))

;; Accept-Encoding = [ ( "," / ( codings [ weight ] ) ) *( OWS "," [ OWS
;;  ( codings [ weight ] ) ] ) ]
(defn accept-encoding [opts]
  (let [codings (codings opts)
        weight (weight opts)]
    {:juxt.reap/decode
     (p/optionally
      (p/cons
       (p/alternatives
        (p/ignore
         (p/pattern-parser
          (re-pattern ",")))
        (p/into
         {}
         (p/sequence-group
          (p/as-entry :juxt.reap.alpha/codings (:juxt.reap/decode codings))
          (p/optionally
           (p/as-entry :juxt.reap.alpha/qvalue (:juxt.reap/decode weight))))))
       (p/zero-or-more
        (p/first
         (p/sequence-group
          (p/ignore
           (p/pattern-parser
            (re-pattern
             (re/re-concat OWS ","))))
          (p/optionally
           (p/first
            (p/sequence-group
             (p/ignore
              (p/pattern-parser
               (re-pattern OWS)))
             (p/into
              {}
              (p/sequence-group
               (p/as-entry :juxt.reap.alpha/codings (:juxt.reap/decode codings))
               (p/optionally
                (p/as-entry :juxt.reap.alpha/qvalue (:juxt.reap/decode weight)))))))))))))}))
