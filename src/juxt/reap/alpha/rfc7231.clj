;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.rfc7231
  (:refer-clojure :exclude [type])
  (:require
   [juxt.reap.alpha.generators :refer [histogram-generator]]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.combinators :as p]
   [juxt.reap.alpha.rfc4647 :as rfc4647]
   [juxt.reap.alpha.rfc7230 :as rfc7230 :refer [OWS RWS token]]
   [juxt.reap.alpha.rfc5234 :as rfc5234 :refer [DIGIT]]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; parameter = token "=" ( token / quoted-string )

(defn- common-parameter
  [optional?]
  (p/as-map
   (p/sequence-group
    [(p/as-entry
       :name
       (p/comp
        str/lower-case
        (p/pattern-parser
         (re-pattern token))))
     (cond->
         (p/first
          (p/sequence-group
           [(p/ignore (p/pattern-parser #"="))
            (p/as-entry
             :value
             (p/alternatives
              (p/pattern-parser (re-pattern token))
              (p/comp
               rfc7230/unescape-quoted-string
               (p/pattern-parser
                (re-pattern rfc7230/quoted-string) {:group 1}))))]))
       optional? (p/optionally))])))

(defn parameter
  "Return a parameter parser that parses into map containing :name
  and :value keys. The :name value is case insensitive and therefore
  converted to lower-case."
  []
  (common-parameter false))

(defn optional-parameter
  "Return a parameter parser that parses into map containing :name and,
  optionally, a :value key. The :name value is case insensitive and
  therefore converted to lower-case."
  []
  (common-parameter true))

;; token = <token, see [RFC7230], Section 3.2.6>

;; subtype = token
(def ^String subtype token)

;; type = token
(def ^String type token)



;; Allow = [ ( "," / method ) *( OWS "," [ OWS method ] ) ]

;; BWS = <BWS, see [RFC7230], Section 3.2.3>

;; Content-Language = *( "," OWS ) language-tag *( OWS "," [ OWS
;;  language-tag ] )
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

;; Vary = "*" / ( *( "," OWS ) field-name *( OWS "," [ OWS field-name ]
;;  ) )

;; absolute-URI = <absolute-URI, see [RFC7230], Section 2.7>

;; accept-ext = OWS ";" OWS token [ "=" ( token / quoted-string ) ]
(defn accept-ext []
  (p/first
   (p/sequence-group
    [(p/ignore
      (p/pattern-parser
       (re-pattern
        (re/re-concat OWS \; OWS))))
     (optional-parameter)])))

;; qvalue = ( "0" [ "." *3DIGIT ] ) / ( "1" [ "." *3"0" ] )
(def qvalue (re/re-compose "0(?:\\.[%s]{0,3})?|1(?:\\.[0]{0,3})?" DIGIT))

;; weight = OWS ";" OWS "q=" qvalue
(defn weight []
  (p/comp
   #(Double/parseDouble %)
   (p/pattern-parser
    (re-pattern
     (re/re-concat
      OWS \; OWS "q=" (re/group qvalue)))
    {:group 1
     :generator (fn []
                  (rand-nth
                   [";q=1.0"
                    ";q=0.9"
                    " ; q=1.0"
                    "; q=0.8"
                    " ;q=0.7"]))})))

(comment
  ((weight) (re/input ";q=0.8")))

;; accept-params = weight *accept-ext
(defn accept-params []
  (p/as-map
   (p/sequence-group
    [(p/as-entry
       :weight
       (weight))
     (p/as-entry
      :accept-ext
      (p/comp
       vec
       (p/seq ; ignore if empty list
        (p/zero-or-more
         (accept-ext)))))])))

(comment
  ((accept-params) (re/input ";foo=bar")))

(comment
  ((accept-params) (re/input ";q=0.8")))

(comment
  ((accept-params) (re/input ";q=0.8;a;b;c=d")))

(comment
  ((p/zero-or-more (accept-ext)) (re/input ";a;b;c=d")))


;; asctime-date = day-name SP date3 SP time-of-day SP year

;; charset = token
(def ^String charset token)

;; content-coding = token
(def ^String content-coding token)

;; Content-Encoding = *( "," OWS ) content-coding *( OWS "," [ OWS
;;  content-coding ] )

(defn content-encoding []
  (p/first
   (p/sequence-group
    [(p/ignore
       (p/zero-or-more
        (p/sequence-group
         [(p/pattern-parser (re-pattern ","))
          (p/pattern-parser (re-pattern OWS))])))
     (p/cons
      (p/pattern-parser (re-pattern content-coding))
      (p/zero-or-more
       (p/first
        (p/sequence-group
         [(p/ignore
           (p/pattern-parser (re-pattern OWS)))
          (p/ignore
           (p/pattern-parser (re-pattern ",")))
          (p/first
           (p/optionally
            (p/sequence-group
             [(p/ignore (p/pattern-parser (re-pattern OWS)))
              (p/pattern-parser (re-pattern content-coding))])))]))))])))

(comment
  ((content-encoding)
   (re/input "foo,bar,zip,qux")
   ))

;; codings = content-coding / "identity" / "*"
(defn codings []
  (p/alternatives
   (p/pattern-parser (re-pattern content-coding) {:generator (fn [] (rand-nth ["utf-8" "UTF8" "Shift_JIS" "US-ASCII"]))})
   (p/pattern-parser (re-pattern "identity") {:generator (constantly "identity")})
   (p/pattern-parser (re-pattern "\\*") {:generator (constantly "*")})))

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

;; language-tag = <Language-Tag, see [RFC5646], Section 2.1>

;; mailbox = <mailbox, see [RFC5322], Section 3.4>

;; quoted-string = <quoted-string, see [RFC7230], Section 3.2.6>


;;(identity rfc7230/quoted-string)

;; media-range = ( "*/*" / ( type "/*" ) / ( type "/" subtype ) ) *( OWS
;;  ";" OWS parameter )

(defn media-range-without-parameters []
  (p/alternatives
   (p/comp
    (fn [_]
      {:media-type "*/*"
       :type "*"
       :subtype "*"})
    (p/pattern-parser
     #"\*/\*"
     {:generator (constantly "*/*")}))
   (p/comp
    (fn [[media-type type]]
      {:media-type (str/lower-case media-type)
       :type (str/lower-case type)
       :subtype "*"})
    (p/pattern-parser
     (re-pattern (re/re-compose "(%s)/\\*" type))
     {:generator (histogram-generator
                  [["application/*" 1]
                   ["audio/*" 1]
                   ["font/*" 1]
                   ["example/*" 1]
                   ["image/*" 2]
                   ["message/*" 1]
                   ["model/*" 1]
                   ["multipart/*" 2]
                   ["text/*" 10]
                   ["video/*" 1]])}))
   (p/comp
    (fn [[media-type type subtype]]
      {:media-type (str/lower-case media-type)
       :type (str/lower-case type)
       :subtype (str/lower-case subtype)})
    (p/pattern-parser
     (re-pattern (re/re-compose "(%s)/(%s)" type subtype))
     {:generator (histogram-generator
                  [["text/html" 10]
                   ["text/csv" 1]
                   ["image/jpeg" 1]
                   ["image/png" 1]
                   ["application/json" 5]
                   ["application/xhtml+xml" 1]])}))))

(defn media-range []
  (p/comp
   #(apply merge %)
   (p/sequence-group
    [(media-range-without-parameters)
     (p/as-map
      (p/list
       (p/as-entry
        :parameters
        (p/zero-or-more
         (p/first
          (p/sequence-group
           [(p/ignore
              (p/pattern-parser
               (re-pattern (re/re-concat OWS \; OWS))))
            (parameter)]))))))])))

(comment
  ((media-range)
   (re/input "text/html ;  foo=bar ;  baz=\"qux;quuz\" ; q=0.9; a=b ; c")))

;; media-type = type "/" subtype *( OWS ";" OWS parameter )
(defn media-type []
  (p/as-map
   (p/sequence-group
    [(p/as-entry
      :type
      (p/pattern-parser (re-pattern type)))
     (p/ignore (p/pattern-parser (re-pattern "/")))
     (p/as-entry
      :subtype
      (p/pattern-parser (re-pattern subtype)))
     (p/as-entry
      :parameters
      (p/zero-or-more
       (p/first
        (p/sequence-group
         [(p/ignore (p/pattern-parser (re-pattern OWS)))
          (p/ignore (p/pattern-parser (re-pattern ";")))
          (p/ignore (p/pattern-parser (re-pattern OWS)))
          (parameter)]))))]
    {:generator (histogram-generator
                 [["text/html" 3]
                  ["image/png" 2]
                  ["text/csv" 1]
                  ["image/jpeg" 1]
                  ["application/json" 3]
                  ["application/xml" 1]])})))

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

(defn product []
  (p/as-map
   (p/sequence-group
    [(p/as-entry
       :product
       (p/pattern-parser (re-pattern token)))
     (p/optionally
      (p/first
       (p/sequence-group
        [(p/ignore
          (p/pattern-parser (re-pattern "/")))
         (p/as-entry
          :version
          (p/pattern-parser (re-pattern product-version)))])))])))

;; Server = product *( RWS ( product / comment ) )

(defn server []
  (p/cons
   (product)
   (p/zero-or-more
    (p/first
     (p/sequence-group
      [(p/ignore (p/pattern-parser (re-pattern RWS)))
       (p/alternatives
        (product)
        #_(rfc7230/rfc-comment))])))))

(comment
  ((server)
   (re/input "foo/1.2 ale1/1.0")))

;; rfc850-date = day-name-l "," SP date2 SP time-of-day SP GMT

;; second = 2DIGIT

;; time-of-day = hour ":" minute ":" second


;; TODO: with re-pattern-parser, return the result of re-groups for post-processing
;; (re-matches #"([a-b]*)/([c-d]*)" "a/c")


;; Accept = [ ( "," / ( media-range [ accept-params ] ) ) *( OWS "," [
;;  OWS ( media-range [ accept-params ] ) ] ) ]
(defn accept []
  (let [media-range-parameter
        (p/first
         (p/sequence-group
          [(p/ignore
             (p/pattern-parser
              (re-pattern (re/re-concat OWS \; OWS))))
           (parameter)]))

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
        parameters-weight-accept-params
        (fn [matcher]
          (loop [matcher matcher
                 result {:parameters []}]
            (if-let [accept-params (accept-params matcher)]
              (merge result accept-params)
              (if-let [match (media-range-parameter matcher)]
                (recur matcher (update result :parameters conj match))
                result))))]
    (p/optionally
     (p/cons
      (p/alternatives
       (p/ignore
        (p/pattern-parser #","))
       (p/comp
        #(apply merge %)
        (p/sequence-group
         [(media-range-without-parameters)
          parameters-weight-accept-params])))
      (p/zero-or-more
       (p/first
        (p/sequence-group
         [(p/ignore
            (p/pattern-parser
             (re-pattern
              (re/re-concat OWS ","))))
          (p/optionally
           (p/first
            (p/sequence-group
             [(p/ignore
                (p/pattern-parser
                 (re-pattern OWS)))
              (p/comp
               #(apply merge %)
               (p/sequence-group
                [(media-range-without-parameters)
                 parameters-weight-accept-params]))])))])))))))

(comment
  ((accept) (re/input "text/html;foo=bar;i=j ; q=0.8;a , application/json;v=10")))

;; year = 4DIGIT

;; Accept-Charset = *( "," OWS ) ( ( charset / "*" ) [ weight ] ) *( OWS
;;  "," [ OWS ( ( charset / "*" ) [ weight ] ) ] )

(defn accept-charset []
  (let [charset-with-weight
        (p/as-map
         (p/sequence-group
          [(p/alternatives
             (p/as-entry
              :charset
              (p/pattern-parser
               (re-pattern charset)))
             (p/pattern-parser
              (re-pattern (re/re-str \*))))
           (p/optionally
            (p/as-entry :weight (weight)))]))]
    (p/cons
     (p/first
      (p/sequence-group
       [(p/ignore
          (p/pattern-parser
           (re-pattern (re/re-compose "(?:%s)*" (re/re-concat \, OWS)))))
        charset-with-weight]))
     (p/zero-or-more
      (p/first
       (p/sequence-group
        [(p/ignore
           (p/pattern-parser
            (re-pattern (re/re-compose "%s%s" OWS ","))))
         (p/optionally
          (p/first
           (p/sequence-group
            [(p/ignore
               (p/pattern-parser
                (re-pattern OWS)))
             charset-with-weight])))]))))))

;; Accept-Language = *( "," OWS ) ( language-range [ weight ] ) *( OWS
;;  "," [ OWS ( language-range [ weight ] ) ] )
(defn accept-language []
  (p/first
   (p/sequence-group
    [(p/ignore
       (p/zero-or-more
        (p/pattern-parser
         (re-pattern
          (re/re-concat "," OWS))
         {:name :pp1
          :generator (constantly ", ")})))
     (p/cons
      (p/as-map
       (p/sequence-group
        [(p/as-entry :language-range (rfc4647/language-range))
         (p/optionally
          (p/as-entry
           :weight
           (weight)))]))
      (p/zero-or-more
       (p/first
        (p/sequence-group
         [(p/ignore
            (p/pattern-parser
             (re-pattern
              (re/re-concat OWS ","))
             {:name :pp2
              :generator (constantly " ,")}))
          (p/optionally
           (p/as-map
            (p/sequence-group
             [(p/ignore
                (p/pattern-parser
                 (re-pattern OWS)
                 {:generator (fn [] (rand-nth ["" " "]))
                  :name :ppu3}))
              (p/as-entry :language-range (rfc4647/language-range))
              (p/optionally
               (p/as-entry :weight (weight)))])))]))))])))

;; Accept-Encoding = [ ( "," / ( codings [ weight ] ) ) *( OWS "," [ OWS
;;  ( codings [ weight ] ) ] ) ]
(defn accept-encoding []
  (p/optionally
   (p/cons
    (p/alternatives
     (p/ignore
      (p/pattern-parser
       (re-pattern ",")))
     (p/as-map
      (p/sequence-group
       [(p/as-entry :codings (codings))
        (p/optionally
         (p/as-entry :weight (weight)))])))
    (p/zero-or-more
     (p/first
      (p/sequence-group
       [(p/ignore
          (p/pattern-parser
           (re-pattern
            (re/re-concat OWS ","))))
        (p/optionally
         (p/first
          (p/sequence-group
           [(p/ignore
              (p/pattern-parser
               (re-pattern OWS)))
            (p/as-map
             (p/sequence-group
              [(p/as-entry :codings (codings))
               (p/optionally
                (p/as-entry :weight (weight)))]))])))]))))))
