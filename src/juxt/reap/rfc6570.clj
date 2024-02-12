;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.rfc6570
  (:require
   [juxt.reap.regex :as re]
   [juxt.reap.decoders.rfc3986 :as rfc3986]
   [juxt.reap.decoders.rfc5234 :as rfc5234]
   [juxt.reap.decoders.rfc6570 :refer [expand uri-template]]
   [juxt.reap.encoders.rfc6570 :refer [pct-encode pct-encode-reserved]]
   [clojure.string :as str]))

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
                ;; "The allowed set for a given expansion depends on
                ;; the expression type: reserved ("+") and
                ;; fragment ("#") expansions allow the set of
                ;; characters in the union of ( unreserved / reserved
                ;; / pct-encoded ) to be passed through without
                ;; pct-encoding" -- RFC 6570 3.2.1. Variable
                ;; Expansion
                \+
                (re/re-compose
                 "((?:[%s]|%s)*?)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved rfc3986/reserved))
                 rfc3986/pct-encoded)

                \#
                (re/re-compose
                 "(?:\\#((?:[%s]|%s)*?))?"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved rfc3986/reserved))
                 rfc3986/pct-encoded)

                ;; ", whereas all other expression types allow only
                ;; unreserved characters to be passed through without
                ;; pct-encoding." -- RFC 6570 3.2.1. Variable
                ;; Expansion
                \.
                (re/re-compose
                 "((?:\\.(?:[%s]|%s)*)*?)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \. \, \=))
                 rfc3986/pct-encoded)

                \/
                (re/re-compose
                 "\\/((?:[%s]|%s)*?)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \, \/ \=))
                 rfc3986/pct-encoded)

                \;
                (re/re-compose
                 "\\;((?:[%s]|%s)*?)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \, \; \=))
                 rfc3986/pct-encoded)

                \?
                (re/re-compose
                 "(?:\\?((?:[%s]|%s)*?))?"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved #{\= \& \,}))
                 rfc3986/pct-encoded)

                \&
                (re/re-compose
                 "\\&((?:[%s]|%s)*?)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved #{\= \& \,}))
                 rfc3986/pct-encoded))

              ;; Default
              (re/re-compose
               "((?:[%s]|%s)*)"
               (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \, \=))
               rfc3986/pct-encoded))))
        components)))}))

(defn var->str [{:keys [varname prefix explode]} operator variables]
  (when-let [val (or
                  (get variables (keyword varname))
                  (get variables varname))]
    (cond
      (map? val)
      (str
       (when-not explode
         (case operator
           (\; \? \&) (str varname "=")
           nil))

       (str/join
        (if explode
          (case operator
            (\. \/ \;) operator
            (\? \&) "&"
            ",")
          ",")

        (for [[k v] val]
          (let [prefixed-val-str
                (cond-> v
                  (and prefix (< prefix (count v)))
                  (subs 0 prefix))]
            (str (name k)
                 (if explode "=" ",")
                 ((case operator
                    (\+ \#) pct-encode-reserved
                    (\. \/ \? \& \;) pct-encode
                    pct-encode) prefixed-val-str))))))

      (sequential? val)
      (when-not (empty? val)
        (str
         (when (and (contains? #{\; \? \&} operator) (not explode))
           (str varname "="))

         (str/join
          (if explode
            (case operator
              (\. \/ \;) operator
              (\? \&) "&"
              ",")
            ",")
          (for [v val]
            (let [prefixed-val-str
                  (cond-> v
                    (and prefix (< prefix (count v)))
                    (subs 0 prefix))]
              (str
               (when (and explode (contains? #{\; \? \&} operator))
                 (str varname "="))

               ((case operator
                  ;; TODO: shouldn't semi-colon be pct-encode?
                  (\+ \# \;) pct-encode-reserved
                  (\. \/ \? \&) pct-encode
                  pct-encode) prefixed-val-str)))))))

      :else
      (let [val (str val)
            prefixed-val-str
            (cond-> val
              (and prefix (< prefix (count val)))
              (subs 0 prefix))]
        (str
         (cond
           (and explode (= operator \;))
           (str varname
                (when-not (str/blank? prefixed-val-str) "="))
           (contains? #{\? \& \;} operator)
           (str varname (if (and (contains? #{\;} operator)
                                 (str/blank? prefixed-val-str))
                          "" "="))
           :else nil)

         ((case operator
            ;; TODO: shouldn't semi-colon be pct-encode?
            (\+ \# \;) pct-encode-reserved
            (\. \/ \? \&) pct-encode
            pct-encode) prefixed-val-str))))))

(defn component->str [{:keys [varlist operator]} variables]
  (let [vals (keep (fn [v] (var->str v operator variables)) varlist)
        s
        (str/join
         (case operator
           (\+) ","
           (\? \&) "&"
           (\/ \; \.) operator
           ",")
         vals)]

    (case operator
      (\? \# \. \/ \; \&) (if-not (empty? vals) (str operator s) s)
      s)))

(defn make-uri [uri-template variables]
  (->
   (reduce
    (fn [acc component]
      (.append acc (if (string? component)
                     component
                     (component->str component variables))))
    (StringBuilder.)
    (:components uri-template))
   (.toString)))

(defn match-uri
  "Given a compiled uri-template (see compile-uri-template) and a URI as
  arguments, return the extracted uri-template expansions if the URI
  matches the uri-template."
  [{:keys [components pattern]} var-types uri]
  (when-let [m (re-matches pattern uri)]
    (if (sequential? m)
      (let [variables
            (map expand
                 (remove string? components)
                 (repeat var-types)
                 (rest m))]
        (apply merge variables))
      ;; Case where there are no variables
      {})))

(comment
  (match-uri
   (compile-uri-template "http://example.com/search{?q,lang}")
   {:q :string :lang :string}
   "http://example.com/search?q=chien&lang=fr"))

(comment
  (match-uri
   (compile-uri-template "http://example.com/~{username}/")
   {:username :string}
   "http://example.com/~mal/"))

(comment
  (match-uri
   (compile-uri-template "http://example.com/")
   {}
   "http://example.com/"))
