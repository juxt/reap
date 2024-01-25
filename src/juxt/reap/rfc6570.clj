;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.rfc6570
  (:require
   [juxt.reap.regex :as re]
   [juxt.reap.decoders.rfc5234 :as rfc5234 :refer [ALPHA DIGIT]]
   [juxt.reap.decoders.rfc6570 :refer [expand uri-template]]
   [juxt.reap.decoders.rfc3986 :as rfc3986]))

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
                 "((?:[%s]|%s)*?)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved rfc3986/reserved))
                 rfc3986/pct-encoded)

                ;; ", whereas all other expression types allow only
                ;; unreserved characters to be passed through without
                ;; pct-encoding." -- RFC 6570 3.2.1. Variable
                ;; Expansion
                \.
                (re/re-compose
                 "\\.((?:[%s]|%s)*)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \. \,))
                 rfc3986/pct-encoded)

                \/
                (re/re-compose
                 "\\/((?:[%s]|%s)*)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \, \/))
                 rfc3986/pct-encoded)

                \;
                (re/re-compose
                 "\\;((?:[%s]|%s)*)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \, \; \=))
                 rfc3986/pct-encoded)

                \?
                (re/re-compose
                 "\\?((?:[%s]|%s)*)"
                 (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved #{\= \&}))
                 rfc3986/pct-encoded))

              ;; Default
              (re/re-compose
               "((?:[%s]|%s)*)"
               (re/re-str (rfc5234/merge-alternatives rfc3986/unreserved \,))
               rfc3986/pct-encoded))))
        components)))}))

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

(comment
  (match-uri
   (compile-uri-template "http://example.com/search?{q,lang}")
   "http://example.com/search?q=chien&lang=fr"))

(comment
  (match-uri
   (compile-uri-template "http://example.com/~{username}/")
   "http://example.com/~mal/"))

(comment
  (match-uri
   (compile-uri-template "http://example.com/")
   "http://example.com/"))
