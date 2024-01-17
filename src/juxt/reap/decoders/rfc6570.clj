;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.decoders.rfc6570
  (:require
   [juxt.reap.regex :as re]
   [juxt.reap.combinators :as p]
   [juxt.reap.decoders.rfc5234 :as rfc5234 :refer [ALPHA DIGIT]]
   [juxt.reap.decoders.rfc3986 :as rfc3986]))

(def op-level2
  (rfc5234/alternatives \+ \#))

(def op-level3
  (rfc5234/alternatives \. \/ \; \? \&))

(def op-reserve
  (rfc5234/alternatives \= \, \! \@ \|))

(def operator
  (p/pattern-parser (re-pattern (re/re-compose "[%s]" (rfc5234/merge-alternatives op-level2 op-level3 op-reserve)))))

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
   (p/as-entry
    :prefix prefix)
   (p/as-entry
    :explode
    (p/comp #(= % "*") explode))))

(def varspec
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     :varname
     (p/pattern-parser
      (re-pattern varname)))
    (p/optionally modifier-level4))))

(comment
  (varspec (re/input "foo:3")))

(def variable-list
  (p/cons
   varspec
   (p/zero-or-more
    (p/first
     (p/sequence-group
      (p/ignore
       (p/pattern-parser #"\,"))
      varspec)))))

(comment
  (variable-list (re/input "foo:4,bar*,zip")))

(def expression
  (p/complete
   (p/into
    {}
    (p/sequence-group
     (p/ignore
      (p/pattern-parser #"\{"))
     (p/optionally
      (p/as-entry
       :operator
       operator))
     (p/as-entry
      :varlist
      (p/comp vec variable-list))
     (p/ignore
      (p/pattern-parser #"\}"))))))

(comment
  (expression (re/input "{foo}")))

(comment
  (expression (re/input "{.foo,zip.fe:4,foo.bar*}")))
