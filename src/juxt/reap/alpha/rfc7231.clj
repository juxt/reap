;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7231
  (:refer-clojure :exclude [type second])
  (:require
   [juxt.reap.alpha :as reap]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.rfc4647 :as rfc4647]
   [juxt.reap.alpha.rfc5234 :as rfc5234 :refer [DIGIT SP]]
   [juxt.reap.alpha.rfc5646 :as rfc5646]
   [juxt.reap.alpha.rfc7230 :as rfc7230 :refer [OWS RWS token quoted-string]]
   [clojure.string :as str]))


(set! *warn-on-reflection* true)

;; Forward references

(declare accept-params)
(declare charset)
(declare codings)
(declare content-coding)
(declare date3)
(declare day)
(declare day-name)
(declare delay-seconds)
(declare hour)
(declare http-date)
(declare imf-fixdate)
(declare language-tag)
(declare media-type)
(declare method)
(declare minute)
(declare month)
(declare parameter)
(declare obs-date)
(declare optional-parameter)
(declare product)
(declare product-version)
(declare rfc850-date)
(declare second)
(declare subtype)
(declare time-of-day)
(declare type)
(declare weight)
(declare year)

;; Helpers

(defn media-range-without-parameters [_]
  (p/alternatives
   (p/comp
    (fn [_]
      #::{:media-range "*/*"
          :type "*"
          :subtype "*"})
    (p/pattern-parser #"\*/\*"))
   (p/comp
    (fn [[media-type type]]
      #::{:media-range media-type
          :type type
          :subtype "*"})
    (p/pattern-parser
     (re-pattern (re/re-compose "(%s)/\\*" type))))
   (p/comp
    (fn [[media-type type subtype]]
      #::{:media-range media-type
          :type type
          :subtype subtype})
    (p/pattern-parser
     (re-pattern (re/re-compose "(%s)/(%s)" type subtype))))))

(defn parameters-map [parser]
  (fn [matcher]
    (when-let [parameters (parser matcher)]
      {::parameters parameters
       ::parameter-map
       (into
        {}
        (map
         (juxt
          (comp
           ;; We lower-case to support case-insensitive lookups
           str/lower-case
           ::parameter-name)
          ::parameter-value)
         parameters))})))

;; Definitions

;; Accept = [ ( "," / ( media-range [ accept-params ] ) ) *( OWS "," [
;;  OWS ( media-range [ accept-params ] ) ] ) ]
(defn ^::reap/codec accept [opts]
  {::reap/decode
   (let [parameter
         (parameter opts)

         media-range-parameter
         (p/first
          (p/sequence-group
           (p/ignore
            (p/pattern-parser
             (re-pattern (re/re-concat OWS \; OWS))))
           (::reap/decode parameter)))

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
                  result {::parameters {}}]
             (if-let [accept-params ((::reap/decode accept-params) matcher)]
               (merge result accept-params)
               (if-let [match (media-range-parameter matcher)]
                 (recur matcher
                        (update
                         result
                         ::parameters
                         conj [(::parameter-name match)
                               (::parameter-value match)]))
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


;; Accept-Charset = *( "," OWS ) ( ( charset / "*" ) [ weight ] ) *( OWS
;;  "," [ OWS ( ( charset / "*" ) [ weight ] ) ] )
(defn ^::reap/codec accept-charset [opts]
  (let [weight (weight opts)]
    {::reap/decode
     (let [charset-with-weight
           (p/into
            {}
            (p/sequence-group
             (p/alternatives
              (p/as-entry
               ::charset
               (p/pattern-parser
                (re-pattern charset)))
              (p/pattern-parser
               (re-pattern (re/re-str \*))))
             (p/optionally
              (p/as-entry ::qvalue (::reap/decode weight)))))]
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


;; Accept-Encoding = [ ( "," / ( codings [ weight ] ) ) *( OWS "," [ OWS
;;  ( codings [ weight ] ) ] ) ]
(defn accept-encoding [opts]
  (let [codings (codings opts)
        weight (weight opts)]
    {::reap/decode
     (p/optionally
      (p/cons
       (p/alternatives
        (p/ignore
         (p/pattern-parser
          (re-pattern ",")))
        (p/into
         {}
         (p/sequence-group
          (p/as-entry ::codings (::reap/decode codings))
          (p/optionally
           (p/as-entry ::qvalue (::reap/decode weight))))))
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
               (p/as-entry ::codings (::reap/decode codings))
               (p/optionally
                (p/as-entry ::qvalue (::reap/decode weight)))))))))))))}))


;; Accept-Language = *( "," OWS ) ( language-range [ weight ] ) *( OWS
;;  "," [ OWS ( language-range [ weight ] ) ] )
(defn ^::reap/codec accept-language [opts]
  (let [weight (weight opts)]
    {::reap/decode
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
           ::rfc4647/language-range
           (rfc4647/language-range opts))
          (p/optionally
           (p/as-entry
            ::qvalue
            (::reap/decode weight)))))
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
               ::rfc4647/language-range
               (rfc4647/language-range opts))
              (p/optionally
               (p/as-entry ::qvalue (::reap/decode weight))))))))))))}))

;; Allow = [ ( "," / method ) *( OWS "," [ OWS method ] ) ]
(defn ^::reap/codec allow [_]
  {::reap/decode
   (p/optionally
    (p/cons
     (p/alternatives
      (p/pattern-parser (re-pattern ","))
      (p/array-map
       ::method
       (p/pattern-parser (re-pattern method))))
     (p/zero-or-more
      (p/first
       (p/sequence-group
        (p/ignore
         (p/pattern-parser (re-pattern OWS)))
        (p/ignore
         (p/pattern-parser (re-pattern ",")))
        (p/optionally
         (p/array-map
          ::method
          (p/first
           (p/sequence-group
            (p/ignore
             (p/pattern-parser (re-pattern OWS)))
            (p/pattern-parser (re-pattern method)))))))))))})

;; BWS = <BWS, see [RFC7230], Section 3.2.3>
;; TODO

;; Content-Encoding = *( "," OWS ) content-coding *( OWS "," [ OWS
;;  content-coding ] )

(defn ^::reap/codec content-encoding [_]
  {::reap/decode
   (p/first
    (p/sequence-group
     (p/ignore
      (p/zero-or-more
       (p/sequence-group
        (p/pattern-parser (re-pattern ","))
        (p/pattern-parser (re-pattern OWS)))))
     (p/cons
      (p/array-map
       ::content-coding
       (p/pattern-parser (re-pattern content-coding)))
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
            (p/array-map
             ::content-coding
             (p/pattern-parser (re-pattern content-coding))))))))))))})

;; Content-Language = *( "," OWS ) language-tag *( OWS "," [ OWS
;;  language-tag ] )
(defn ^::reap/codec content-language [opts]
  (let [language-tag (language-tag opts)]
    {::reap/decode
     (p/first
      (p/sequence-group
       (p/ignore
        (p/zero-or-more
         (p/sequence-group
          (p/pattern-parser (re-pattern ","))
          (p/pattern-parser (re-pattern OWS)))))
       (p/cons
        (::reap/decode language-tag)
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
              (::reap/decode language-tag))))))))))}))

;; Content-Location = absolute-URI / partial-URI
;; TODO

;; Content-Type = media-type
(def content-type #'media-type)

;; Date = HTTP-date
(def date http-date)

;; Expect = "100-continue"
(def expect "100-continue")

;; From = mailbox
;; TODO

;; GMT
(def ^String GMT (re/re-str (map #(format "\\x%02X" %) (map int "GMT"))))

;; HTTP-date = IMF-fixdate / obs-date
(defn ^::reap/codec http-date [opts]
  (let [imf-fixdate (imf-fixdate opts)
        obs-date (obs-date opts)]
    {::reap/decode
     (p/alternatives
      (::reap/decode imf-fixdate)
      (::reap/decode obs-date))
     ::reap/encode
     (fn [http-date]
       ((::reap/encode imf-fixdate)
        http-date)
       )}))

;; IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT
(defn ^::reap/codec imf-fixdate [_]
  (let [formatter (.withZone
                   java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME
                   (java.time.ZoneId/of "GMT"))]
    {::reap/decode
     (p/comp
      ;; TODO: Do the same for for obs-date
      (fn [m]
        (assoc
         m ::date
         (some-> m
                 ::imf-fixdate
                 (java.time.ZonedDateTime/parse formatter)
                 java.time.Instant/from
                 java.util.Date/from)))
      (p/pattern-parser
       (re-pattern
        (str
         (format "(?<dayname>%s)" day-name)
         (re/re-concat "," SP)
         ;; Strictly speaking, we should insist on a 2-digit day
         ;; number. However, java.time.format.DateTimeFormat/RFC_1123_DATE_TIME
         ;; generate days with 1 digit, for single digit days. There could be
         ;; other examples of date formatters that don't strictly conform. RFC
         ;; 7231 states "Recipients of timestamp values are encouraged to be
         ;; robust in parsing timestamps unless otherwise restricted by the
         ;; field definition.  For example, messages are occasionally forwarded
         ;; over HTTP from a non-HTTP source that might generate any of the date
         ;; and time specifications defined by the Internet Message
         ;; Format.". Therefore, we relax the DIGIT{2} of day to use DIGIT{1,2}
         ;; here instead. In future, reap could include a 'strict' option to
         ;; switch between enforcing strict conformance and being more
         ;; permissive (see Postel's Law:
         ;; https://en.wikipedia.org/wiki/Robustness_principle).
         (format "(?<day>%s)" (re/re-compose "%s{1,2}" DIGIT))
         SP
         (format "(?<month>%s)" month)
         SP
         (format "(?<year>%s)" year)
         SP
         (re/re-compose "(?<hour>%s):(?<minute>%s):(?<second>%s)" hour minute second)
         SP
         GMT))
       {:group
        #::{:imf-fixdate 0
            :day-name "dayname"
            :day "day"
            :month "month"
            :year "year"
            :hour "hour"
            :minute "minute"
            :second "second"}}))
     ::reap/encode
     (fn [m]
       (when-let [^java.util.Date date (::date m)]
         (. formatter format (. date toInstant))))}))

;; obsolete

;; Location = URI-reference
;; TODO

;; Max-Forwards = 1*DIGIT
(def max-forwards (re/re-compose "%s+" DIGIT))

;; OWS = <OWS, see [RFC7230], Section 3.2.3>

;; RWS = <RWS, see [RFC7230], Section 3.2.3>

;; Referer = absolute-URI / partial-URI
;; TODO

;; Retry-After = HTTP-date / delay-seconds
(defn ^::reap/codec retry-after [opts]
  (let [http-date (http-date opts)]
    {::reap/decode
     (p/alternatives
      (::reap/decode http-date)
      (p/array-map
       ::delay-seconds
       (p/pattern-parser (re-pattern delay-seconds))))}))

;; Server = product *( RWS ( product / comment ) )
(defn ^::reap/codec server [opts]
  {::reap/decode
   (let [product (product opts)]
     (p/cons
      (::reap/decode product)
      (p/zero-or-more
       (p/first
        (p/sequence-group
         (p/ignore (p/pattern-parser (re-pattern RWS)))
         (p/alternatives
          (::reap/decode product)
          #_(rfc7230/rfc-comment)))))))})

;; URI-reference = <URI-reference, see [RFC7230], Section 2.7>
;; TODO

;; User-Agent = product *( RWS ( product / comment ) )
;; TODO

;; Vary = "*" / ( *( "," OWS ) field-name *( OWS "," [ OWS field-name ] ) )
(defn ^::reap/codec vary [_]
  {::reap/decode
   (p/alternatives
    (p/array-map
     ::wildcard
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
        ::field-name
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
            ::field-name
            (p/pattern-parser (re-pattern rfc7230/field-name)))))))))))
   ::reap/encode
   (fn vary-str [decoded]
     (cond
       (and (map? decoded) (contains? decoded ::wildcard))
       "*"
       (sequential? decoded)
       (->>
        (for [i decoded]
          (::field-name i))
        (str/join ", "))
       :else (throw (ex-info "Unrecognised vary data" {:arg decoded}))))})

;; absolute-URI = <absolute-URI, see [RFC7230], Section 2.7>
;; TODO

;; accept-ext = OWS ";" OWS token [ "=" ( token / quoted-string ) ]
(defn ^::reap/codec accept-ext [opts]
  {::reap/decode
   (let [optional-parameter (optional-parameter opts)]
     (p/first
      (p/sequence-group
       (p/ignore
        (p/pattern-parser
         (re-pattern
          (re/re-concat OWS \; OWS))))
       (::reap/decode optional-parameter))))})

;; accept-params = weight *accept-ext
(defn ^::reap/codec accept-params [opts]
  {::reap/decode
   (let [weight (weight opts)
         accept-ext (accept-ext opts)]
     (p/into
      {}
      (p/sequence-group
       (p/as-entry
        ::qvalue
        (::reap/decode weight))
       (p/as-entry
        ::accept-ext
        (p/comp
         vec
         (p/seq ; ignore if empty list
          (p/zero-or-more
           (::reap/decode accept-ext))))))))})

;; asctime-date = day-name SP date3 SP time-of-day SP year
(defn ^::reap/codec asctime-date [_]
  {::reap/decode
   (p/pattern-parser
    (re-pattern
     (str
      (format "(?<dayname>%s)" day-name)
      SP
      ;; date3
      (re-pattern
       (str
        (format "(?<month>%s)" month)
        SP
        (re/re-compose "(?<day>%s{2}|%s%s)" DIGIT SP DIGIT)))
      SP
      (re/re-compose "(?<hour>%s):(?<minute>%s):(?<second>%s)" hour minute second)
      SP
      (format "(?<year>%s)" year)))
    {:group
     #::{:asctime-date 0
         :day-name "dayname"
         :day "day"
         :month "month"
         :year "year"
         :hour "hour"
         :minute "minute"
         :second "second"}})})

;; charset = token
(def ^String charset token)

;; codings = content-coding / "identity" / "*"
(defn ^::reap/codec codings [_]
  {::reap/decode
   (p/alternatives
    (p/pattern-parser (re-pattern content-coding))
    (p/pattern-parser (re-pattern "identity"))
    (p/pattern-parser (re-pattern "\\*")))})

;; comment = <comment, see [RFC7230], Section 3.2.6>
;; TODO

;; content-coding = token / "identity" / "*"
;; TODO: we're just check this accepting 'identity' and '*' as a token, is this right?
(def ^String content-coding token)

;; date1 = day SP month SP year
(defn ^::reap/codec date1 [_]
  {::reap/decode
   (p/pattern-parser
    (re-pattern
     (str
      (format "(?<day>%s)" day)
      SP
      (format "(?<month>%s)" month)
      SP
      (format "(?<year>%s)" year)))
    {:group
     #::{:day "day"
         :month "month"
         :year "year"}})})

;; date2 = day "-" month "-" 2DIGIT
(defn ^::reap/codec date2 [_]
  {::reap/decode
   (p/pattern-parser
    (re-pattern
     (str
      (format "(?<day>%s)" day)
      "-"
      (format "(?<month>%s)" month)
      "-"
      (re/re-compose "(?<year>%s{2})" DIGIT)))
    {:group
     #::{:day "day"
         :month "month"
         :year "year"}})})

;; date3 = month SP ( 2DIGIT / ( SP DIGIT ) )
(defn ^::reap/codec date3 [_]
  {::reap/decode
   (p/pattern-parser
    (re-pattern
     (str
      (format "(?<month>%s)" month)
      SP
      (re/re-compose "(?<day>%s{2}|%s%s)" DIGIT SP DIGIT)))
    {:group
     #::{:day "day"
         :month "month"}})})

;; day = 2DIGIT
(def ^String day (re/re-compose "%s{2}" DIGIT))

;; day-name = %x4D.6F.6E ; Mon
;;  / %x54.75.65 ; Tue
;;  / %x57.65.64 ; Wed
;;  / %x54.68.75 ; Thu
;;  / %x46.72.69 ; Fri
;;  / %x53.61.74 ; Sat
;;  / %x53.75.6E ; Sun
(def ^String day-name
  (str/join
   "|"
   ;; There's a good reason for converting these strings into hex, as the ABNF
   ;; does. If this string is used in a compound regex which is configured to
   ;; ignore-case, we want these patterns to be exact.
   (for [day ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]]
     (re/re-str
      (map #(format "\\x%02X" %) (map int day))))))

;; day-name-l = %x4D.6F.6E.64.61.79 ; Monday
;;  / %x54.75.65.73.64.61.79 ; Tuesday
;;  / %x57.65.64.6E.65.73.64.61.79 ; Wednesday
;;  / %x54.68.75.72.73.64.61.79 ; Thursday
;;  / %x46.72.69.64.61.79 ; Friday
;;  / %x53.61.74.75.72.64.61.79 ; Saturday
;;  / %x53.75.6E.64.61.79 ; Sunday
(def ^String day-name-1
  (str/join
   "|"
   (for [day ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"]]
     (re/re-str
      (map #(format "\\x%02X" %) (map int day))))))

;; delay-seconds = 1*DIGIT
(def delay-seconds (re/re-compose "%s+" DIGIT))

;; field-name    = <field-name, see [RFC7230], Section 3.2>

;; As per verified errata (https://www.rfc-editor.org/errata_search.php?rfc=7231)

;; hour = 2DIGIT
(def ^String hour (re/re-compose "%s{2}" DIGIT))

;; language-range = <language-range, see [RFC4647], Section 2.1>
(def language-range rfc4647/language-range)

;; language-tag = <Language-Tag, see [RFC5646], Section 2.1>
(def language-tag rfc5646/language-tag)

;; mailbox = <mailbox, see [RFC5322], Section 3.4>
;; TODO

;; media-range = ( "*/*" / ( type "/*" ) / ( type "/" subtype ) ) *( OWS
;;  ";" OWS parameter )
(defn ^::reap/codec media-range [opts]
  {::reap/decode
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
           (::reap/decode parameter))))))))})

;; media-type = type "/" subtype *( OWS ";" OWS parameter )
(defn ^::reap/codec media-type [opts]
  {::reap/decode
   (let [parameter (parameter opts)]
     (p/into
      {}
      (p/sequence-group
       (p/as-entry
        ::type
        (p/pattern-parser (re-pattern type)))
       (p/ignore (p/pattern-parser (re-pattern "/")))
       (p/as-entry
        ::subtype
        (p/pattern-parser (re-pattern subtype)))
       (parameters-map
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore (p/pattern-parser (re-pattern OWS)))
           (p/ignore (p/pattern-parser (re-pattern ";")))
           (p/ignore (p/pattern-parser (re-pattern OWS)))
           (::reap/decode parameter))))))))})

;; method = token
(def ^String method token)

;; minute = 2DIGIT
(def ^String minute (re/re-compose "%s{2}" DIGIT))

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
(def month
  (str/join
   "|"
   (for [day ["Jan" "Feb" "Mar"
              "Apr" "May" "Jun"
              "Jul" "Aug" "Sep"
              "Oct" "Nov" "Dec"]]
     (re/re-str
      (map #(format "\\x%02X" %) (map int day))))))

;; obs-date = rfc850-date / asctime-date
(defn ^::reap/codec obs-date [opts]
  {::reap/decode
   (p/alternatives
    (::reap/decode (rfc850-date opts))
    (::reap/decode (asctime-date opts)))})

;; TODO: https://tools.ietf.org/html/rfc7231#section-7.1.1.1
;; Recipients of a timestamp value in rfc850-date format, which uses a
;; two-digit year, MUST interpret a timestamp that appears to be more
;; than 50 years in the future as representing the most recent year in
;; the past that had the same last two digits.

;; parameter = token "=" ( token / quoted-string )
(defn ^::reap/codec parameter
  "Return a parameter parser that parses into map containing :name
  and :value keys."
  [opts]
  {::reap/decode
   (p/into
    {}
    (p/sequence-group
     (p/as-entry
      ::parameter-name
      (p/lower-case
       opts
       (p/pattern-parser
        (re-pattern token))))
     (p/first
      (p/sequence-group
       (p/ignore (p/pattern-parser #"="))
       (p/as-entry
        ::parameter-value
        (p/alternatives
         (p/pattern-parser (re-pattern token))
         (p/comp
          rfc7230/unescape-quoted-string
          (p/pattern-parser
           (re-pattern quoted-string) {:group 1}))))))))})

(defn ^::reap/codec optional-parameter
  "Return a parameter parser that parses into map containing :name and,
  optionally, a :value key."
  [_]
  {::reap/decode
   (p/into
    {}
    (p/sequence-group
     (p/as-entry
      ::parameter-name
      (p/pattern-parser
       (re-pattern token)))
     (p/optionally
      (p/first
       (p/sequence-group
        (p/ignore (p/pattern-parser #"="))
        (p/as-entry
         ::parameter-value
         (p/alternatives
          (p/pattern-parser (re-pattern token))
          (p/comp
           rfc7230/unescape-quoted-string
           (p/pattern-parser
            (re-pattern quoted-string) {:group 1})))))))))})

;; partial-URI = <partial-URI, see [RFC7230], Section 2.7>
;; TODO

;; product = token [ "/" product-version ]
(defn ^::reap/codec product [_]
  {::reap/decode
   (p/into
    {}
    (p/sequence-group
     (p/as-entry
      ::product
      (p/pattern-parser (re-pattern token)))
     (p/optionally
      (p/first
       (p/sequence-group
        (p/ignore
         (p/pattern-parser (re-pattern "/")))
        (p/as-entry
         ::version
         (p/pattern-parser (re-pattern product-version))))))))})

;; product-version = token
(def product-version token)

;; quoted-string = <quoted-string, see [RFC7230], Section 3.2.6>

;; qvalue = ( "0" [ "." *3DIGIT ] ) / ( "1" [ "." *3"0" ] )
(def qvalue (re/re-compose "(?:0(?:\\.%s{0,3})?|1(?:\\.[0]{0,3})?)(?![0-9\\.])" DIGIT))

;; rfc850-date = day-name-l "," SP date2 SP time-of-day SP GMT
(defn ^::reap/codec rfc850-date [_]
  {::reap/decode
   (p/pattern-parser
    (re-pattern
     (str
      (format "(?<dayname>%s)" day-name-1)
      (re/re-concat "," SP)
      (format "(?<day>%s)" day)
      "-"
      (format "(?<month>%s)" month)
      "-"
      (re/re-compose "(?<year>%s{2})" DIGIT)
      SP
      (re/re-compose "(?<hour>%s):(?<minute>%s):(?<second>%s)" hour minute second)
      SP
      GMT))
    {:group
     #::{:rfc850-date 0
         :day-name "dayname"
         :day "day"
         :month "month"
         :year "year"
         :hour "hour"
         :minute "minute"
         :second "second"}})})

;; second = 2DIGIT
(def ^String second (re/re-compose "%s{2}" DIGIT))

;; subtype = token
(def ^String subtype token)

;; time-of-day = hour ":" minute ":" second
(def ^String time-of-day (re/re-compose "%s:%s:%s" hour minute second))

;; token = <token, see [RFC7230], Section 3.2.6>

;; type = token
(def ^String type token)

;; weight = OWS ";" OWS "q=" qvalue
(defn ^::reap/codec weight [_]
  {::reap/decode
   (p/comp
    #(Double/parseDouble %)
    (p/pattern-parser
     (re-pattern
      (re/re-concat
       OWS \; OWS "(?i:q=" (re/group qvalue) ")"))
     {:group 1}))})

;; year = 4DIGIT
(def ^String year (re/re-compose "%s{4}" DIGIT))


;; Rich comments, for promoting into tests

(comment
  ((p/zero-or-more (::reap/decode (accept-ext {}))) (re/input ";a;b;c=d")))

(comment
  ((::reap/decode (weight {})) (re/input ";q=0.8")))

(comment
  ;; This will return nil, since accept-params must start with a qvalue.
  ((::reap/decode (accept-params {})) (re/input ";foo=bar")))

(comment
  ((::reap/decode (accept-params {})) (re/input ";q=0.8")))

(comment
  ((::reap/decode (accept-params {})) (re/input ";q=0.8;a;b;c=d")))

(comment
  ((::reap/decode (content-encoding {}))
   (re/input ",,,, , , foo,zip,qux")))

(comment
  (re-matches day-name "Mon"))

(comment
  ((::reap/decode (media-range {}))
   (re/input "text/html ;  foo=bar ;  baz=\"qux;quuz\" ; q=0.9; a=b ; c")))

(comment
  ((::reap/decode (server {}))
   (re/input "foo/1.2 ale1/1.0")))
