;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.rfc7231
  (:refer-clojure :exclude [type])
  (:require
   [juxt.reap.regex :as re]
   [juxt.reap.parse :as p]
   [juxt.reap.rfc7230 :as rfc7230 :refer [OWS token]]
   [juxt.reap.rfc5234 :as rfc5234 :refer [DIGIT]]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- common-parameter
  "A shared implementation of parameter parsing that can support
  different patterns. Patterns must yield at least 3 groups (name,
  value and, optionally, quoted-string)."
  [pattern _]
  (fn [matcher]
    (.usePattern ^java.util.regex.Matcher matcher pattern)
    (when-let [[_ n v qs] (re-find matcher)]
      (re/advance-and-return
       matcher
       (apply conj {:name (str/lower-case n)}
              (cond v [[:value v]]
                    qs [[:value (rfc7230/unescape-quoted-string qs)]
                        [:raw-value (str "\"" qs "\"")]]))))))

;; parameter = token "=" ( token / quoted-string )
(defn parameter
  "Matches a parameter and returns a map containing :name and :value
  entries. If the value is in a quoted string, this matcher extracts
  the content inside the quoted-string to return as the value."
  ([] (parameter {}))
  ([opts]
   (common-parameter
    (re-pattern
     (re/re-compose
      "(%s)=(?:(%s)|(?:%s))"
      token token rfc7230/quoted-string))
    opts)))

(defn optional-parameter
  "Matches an optional parameter and returns a map containing :name
  and :value entries. If the value is in a quoted string, this matcher
  extracts the content inside the quoted-string to return as the
  value."
  ([] (optional-parameter {}))
  ([opts]
   (common-parameter
    (re-pattern
     (re/re-compose
      "(%s)(?:=(?:(%s)|(?:%s)))?"
      token token rfc7230/quoted-string))
    opts)))




;; token = <token, see [RFC7230], Section 3.2.6>

;; subtype = token
(def ^String subtype token)

;; type = token
(def ^String type token)


;; Accept-Encoding = [ ( "," / ( codings [ weight ] ) ) *( OWS "," [ OWS
;;  ( codings [ weight ] ) ] ) ]
;; Accept-Language = *( "," OWS ) ( language-range [ weight ] ) *( OWS
;;  "," [ OWS ( language-range [ weight ] ) ] )
;; Allow = [ ( "," / method ) *( OWS "," [ OWS method ] ) ]

;; BWS = <BWS, see [RFC7230], Section 3.2.3>

;; Content-Encoding = *( "," OWS ) content-coding *( OWS "," [ OWS
;;  content-coding ] )
;; Content-Language = *( "," OWS ) language-tag *( OWS "," [ OWS
;;  language-tag ] )
;; Content-Location = absolute-URI / partial-URI
;; Content-Type = media-type

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

;; Server = product *( RWS ( product / comment ) )

;; URI-reference = <URI-reference, see [RFC7230], Section 2.7>
;; User-Agent = product *( RWS ( product / comment ) )

;; Vary = "*" / ( *( "," OWS ) field-name *( OWS "," [ OWS field-name ]
;;  ) )

;; absolute-URI = <absolute-URI, see [RFC7230], Section 2.7>

;; accept-ext = OWS ";" OWS token [ "=" ( token / quoted-string ) ]
(defn accept-ext []
  (let [parser
        (p/sequence-group
         (p/pattern-parser (re-pattern (re/re-concat OWS \; OWS)))
         (optional-parameter))]
    (fn [matcher]
      (parser matcher))))

;; qvalue = ( "0" [ "." *3DIGIT ] ) / ( "1" [ "." *3"0" ] )
(def qvalue (re/re-compose "0(?:\\.[%s]{0,3})?|1(?:\\.[0]{0,3})?" DIGIT))

;; weight = OWS ";" OWS "q=" qvalue
(defn weight []
  (let [pat (re-pattern (re/re-concat OWS \; OWS "q=" (re/group qvalue)))]
    (fn [matcher]
      (.usePattern ^java.util.regex.Matcher matcher pat)
      (when (.lookingAt ^java.util.regex.Matcher matcher)
        (Float/parseFloat (re/advance-and-return matcher (.group ^java.util.regex.Matcher matcher 1)))))))

;; accept-params = weight *accept-ext
(defn accept-params []
  (let [parser
        (p/sequence-group
         (weight)
         (p/sequence
          (keep second) ; transducer
          (p/zero-or-more (accept-ext))))]
    (fn [matcher]
      (when-let [[weight accept-ext] (parser matcher)]
        {:weight weight
         :accept-ext (vec accept-ext)}))))


(comment
  ((accept-params) (re/input " ; q=0.3;foo=bar;zip")))

;; asctime-date = day-name SP date3 SP time-of-day SP year

;; charset = token
(def ^String charset token)

;; codings = content-coding / "identity" / "*"
;; comment = <comment, see [RFC7230], Section 3.2.6>
;; content-coding = token

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

;; field-name = <comment, see [RFC7230], Section 3.2>

;; hour = 2DIGIT

;; language-range = <language-range, see [RFC4647], Section 2.1>
;; language-tag = <Language-Tag, see [RFC5646], Section 2.1>

;; mailbox = <mailbox, see [RFC5322], Section 3.4>

;; quoted-string = <quoted-string, see [RFC7230], Section 3.2.6>


;;(identity rfc7230/quoted-string)


;; media-range = ( "*/*" / ( type "/*" ) / ( type "/" subtype ) ) *( OWS
;;  ";" OWS parameter )
(def media-range-result
  (some-fn
   #(when-let [type (get % 1)]
      {:type (str/lower-case type) :subtype (str/lower-case (get % 2))})
   #(when-let [type (get % 3)]
      {:type (str/lower-case type) :subtype (str/lower-case (get % 4))})
   #(when-let [type (get % 5)]
      {:type (str/lower-case type) :subtype (str/lower-case (get % 6))})))

(defn media-range
  ([] (media-range {}))
  ([opts]
   (let [media-range-pattern
         (re-pattern
          (re/re-compose
           "%s|%s|%s"
           (str "(\\*)/(\\*)")
           (format "(%s)/(\\*)" (re/re-str type))
           (format "(%s)/(%s)" (re/re-str type) (re/re-str type))))

         parameter-prefix
         (p/pattern-parser (re-pattern (re/re-concat OWS \; OWS)))

         parameters
         (->> (p/zero-or-more
               (p/sequence-group
                parameter-prefix
                (parameter opts)))
              (p/sequence (keep second)))]

     (fn [matcher]
       (when-let [result
                  (media-range-result
                   (re/re-find-with-pattern matcher media-range-pattern))]

         [:media-range
          (conj
           (re/advance-and-return matcher result)
           [:params (parameters matcher)])])))))

(comment
  ((media-range)
   (re/input "text/html ;  foo=bar ;  baz=\"qux;quuz\" ; q=0.9; a=b ; c")))

;; media-type = type "/" subtype *( OWS ";" OWS parameter )

;; method = token
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
;; product = token [ "/" product-version ]
;; product-version = token

;; rfc850-date = day-name-l "," SP date2 SP time-of-day SP GMT

;; second = 2DIGIT

;; time-of-day = hour ":" minute ":" second


;; Accept = [ ( "," / ( media-range [ accept-params ] ) ) *( OWS "," [
;;  OWS ( media-range [ accept-params ] ) ] ) ]

(defn accept []
  (let [media-range-pattern
        (re-pattern
         (re/re-compose
          "%s|%s|%s"
          (str "(\\*)/(\\*)")
          (format "(%s)/(\\*)" (re/re-str type))
          (format "(%s)/(%s)" (re/re-str type) (re/re-str type))))

        media-range-parameter-prefix
        (p/pattern-parser (re-pattern (re/re-concat OWS \; OWS)))

        media-range-parameter-pattern
        (p/sequence-group media-range-parameter-prefix (parameter))

        accept-params (accept-params)

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
        parameters
        (fn this [matcher]
          (loop [matcher matcher
                 result {:parameters []}]
            (if-let [accept-params (accept-params matcher)]
              (merge result accept-params)
              (if-let [match (media-range-parameter-pattern matcher)]
                (recur matcher (update result :parameters conj (second match)))
                result))))

        media-range-with-accept-params-parser
        (fn [matcher]
          (when-let
              [result
               (media-range-result
                (re/re-find-with-pattern matcher media-range-pattern))]
              (into
               {}
               (concat
                (re/advance-and-return matcher result)
                (parameters matcher)))))]

    (p/cons
     (p/alternatives
      (p/ignore (p/pattern-parser #","))
      media-range-with-accept-params-parser)
     (p/zero-or-more
      (p/first
       (p/sequence-group
        (p/ignore (p/pattern-parser (re-pattern (re/re-concat OWS ","))))
        (p/optionally
         (p/first
          (p/sequence-group
           (p/ignore (p/pattern-parser (re-pattern OWS)))
           media-range-with-accept-params-parser)))))))))


;; year = 4DIGIT




;; Accept-Charset = *( "," OWS ) ( ( charset / "*" ) [ weight ] ) *( OWS
;;  "," [ OWS ( ( charset / "*" ) [ weight ] ) ] )
(let [parser
      (let [charset-with-weight
            (p/as-map
             (p/sequence-group
              (p/alternatives
               (p/as-entry
                :charset
                (p/pattern-parser
                 (re-pattern charset)))
               (p/pattern-parser
                (re-pattern (re/re-str \*))))
              (p/optionally
               (p/as-entry
                :weight (weight)))))]
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
               charset-with-weight))))))))]
  (criterium.core/quick-bench
   (let [matcher (re/input ", \t, , , UTF-8;q=0.8,shift_JIS;q=0.4,a,b")]
     (parser matcher))))

(criterium.core/quick-bench
   (let [matcher (re/input ", \t, , , UTF-8;q=0.8,shift_JIS;q=0.4,a,b")]
     (parser matcher)))

(require 'criterium.core)

#_(let [p (accept)]
  (criterium.core/quick-bench
   (p
    (re/input "text/html;charset=utf-8;q=0.3,text/xml;q=1"))))
