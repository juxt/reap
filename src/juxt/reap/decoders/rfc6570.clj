;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.decoders.rfc6570
  (:require
   [juxt.reap.regex :as re]
   [juxt.reap.interval :as i]
   [juxt.reap.combinators :as p]
   [juxt.reap.decoders.rfc5234 :as rfc5234 :refer [ALPHA DIGIT]]
   [juxt.reap.decoders.rfc3986 :as rfc3986]
   [clojure.string :as str]))

(def op-level2
  (rfc5234/alternatives \+ \#))

(def op-level3
  (rfc5234/alternatives \. \/ \; \? \&))

(def op-reserve
  (rfc5234/alternatives \= \, \! \@ \|))

(def operator
  (re/re-compose
   "[%s]"
   (rfc5234/merge-alternatives
    op-level2
    op-level3
    op-reserve)))

(def varchar
  (re/re-compose
   "(?:[%s]|%s)"
   (rfc5234/merge-alternatives ALPHA DIGIT #{\_})
   rfc3986/pct-encoded))

(def varname
  (re/re-compose
   "(?:%s(?:\\.?%s)*)"
   varchar varchar))

(def prefix
  (p/first
   (p/sequence-group
    (p/ignore
     (p/pattern-parser #"\:"))
    (p/comp
     #(Long/parseLong %)
     (p/pattern-parser #"[0-9]{0,3}")))))

(def explode
  (p/pattern-parser #"\*"))

(def modifier-level4
  (p/alternatives
   (p/as-entry :prefix prefix)
   (p/as-entry :explode (p/comp #(= % "*") explode))))

(def varspec
  (p/into
   {}
   (p/sequence-group
    (p/as-entry :varname (p/pattern-parser (re-pattern varname)))
    (p/optionally modifier-level4))))

(comment
  (varspec (re/input "foo:3")))

(comment
  (varspec (re/input "foo*")))

(def variable-list
  (p/comp
   vec
   (p/cons
    varspec
    (p/zero-or-more
     (p/first
      (p/sequence-group
       (p/ignore (p/pattern-parser #"\,"))
       varspec))))))

(comment
  (variable-list (re/input "foo:4,bar*,zip")))

(def expression
  (p/into
   {}
   (p/sequence-group
    (p/ignore (p/pattern-parser #"\{"))
    (p/optionally
     (p/as-entry :operator (p/comp first (p/pattern-parser (re-pattern operator)))))
    (p/as-entry :varlist variable-list)
    (p/ignore (p/pattern-parser #"\}")))))

(comment
  (expression (re/input "{foo}")))

(comment
  (expression (re/input "{.foo,zip.fe:4,foo.bar*}")))

(def literals
  (re/re-compose
   "(?:[%s]|%s)"
   (rfc5234/alternatives
    0x21
    (i/->interval [0x23 0x24])
    0x26
    (i/->interval [0x28 0x3B])
    0x3D
    (i/->interval [0x3F 0x5B])
    0x5D
    0x5F
    (i/->interval [0x61 0x7A])
    0x7E
    ;; TODO: ucschar
    ;; TODO: iprivate
    )
   rfc3986/pct-encoded))

(def uri-template
  (p/complete
   (p/zero-or-more
    (p/alternatives
     (p/pattern-parser (re-pattern (re/re-compose "%s+" literals)))
     expression))))

(comment
  (uri-template (re/input "http://example.com/~{username}/"))
  (uri-template (re/input "http://example.com/dictionary/{term:1}/{term}"))
  (uri-template (re/input "http://example.com/search{?q,lang}")))

(comment
  '("http://example.com/~" {:varlist [{:varname "username"}]} "/"))

(comment
  (uri-template (re/input "http://example.com/~{username}/{file}{.suffix}")))

(defn compile-uri-template [uri-template-str]
  (let [components (uri-template (re/input uri-template-str))]
    {:components components
     :pattern
     (re-pattern
      (apply
       str
       (map
        (fn [component]
          (if (string? component)
            (format "\\Q%s\\E" component)
            (if-let [op (:operator component)]
              (case op
                \?
                (re/re-compose
                 "\\?((?:[%s]|%s)*)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved #{\= \&}))
                 rfc3986/pct-encoded)
                \.
                (re/re-compose
                 "\\.((?:[%s]|%s)*)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \.))
                 rfc3986/pct-encoded)
                )
              ;; Default
              (re/re-compose
               "((?:[%s]|%s)*)"
               (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \,))
               rfc3986/pct-encoded))))
        components)))}))

(defn expand [{:keys [varlist operator explode] :as expression} expansion]
  (if operator
    (case operator
      \?
      (let [params (into {} (map #(str/split % #"=")
                                 (str/split expansion #"&")))]
        (into {} (map (fn [k] [(:varname k) (get params (:varname k))]) varlist)))

      \.
      (into {} (map (fn [k v] [(:varname k) v])
                      varlist (str/split expansion #"\.")))

      (throw (ex-info "Unsupported operator" {:operator operator})))

    ;; default
    (into {} (map (fn [k v] [(:varname k) v])
                  varlist (str/split expansion #",")))))

(defn match-uri
  "Given a compiled uri-template (see compile-uri-template) and a URI as
  arguments, return the extracted uri-template expansions if the URI
  matches the uri-template."
  [{:keys [components pattern] :as compiled-uri-template} uri]
  (when-let [m (re-matches pattern uri)]
    (if (sequential? m)
      (let [variables
            (map expand
                 (remove string? components)
                 (rest m))]
        {:uri (first m)
         :vars (apply merge variables)})
      {:uri m})))

(compile-uri-template "http://example.com/search{?q,lang}")

(match-uri
 (compile-uri-template "http://example.com/search?{q,lang}")
 "http://example.com/search?q=chien&lang=fr")

(comment
  (match-uri
   (compile-uri-template "http://example.com/~{username}/")
   "http://example.com/~mal/"))

(comment
  (match-uri
   (compile-uri-template "http://example.com/")
   "http://example.com/"))