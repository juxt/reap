;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.decoders.rfc6570
  (:require
   [juxt.reap.regex :as re]
   [juxt.reap.interval :as i]
   [juxt.reap.combinators :as p]
   [juxt.reap.decoders.rfc5234 :as rfc5234 :refer [ALPHA DIGIT]]
   [juxt.reap.decoders.rfc3986 :as rfc3986]
   [clojure.string :as str])
  (:import
   (java.net URLDecoder)))

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

(comment
  (uri-template (re/input "http://example.com/search{?q,lang}")))

(defn distribute-values
  "Return the varlist augmented with values. If there are the same
  number of values as vars in the varlist, then each var will be
  associated with a value. If there are any extra values, these will
  be given to any var that has an explode modifier."
  [varlist vals]
  (when (> (count (filter :explode varlist)) 1)
    (throw (ex-info "Cannot have multiple vars that have explode modifier set" {:varlist varlist})))
  (let [extra (- (count vals) (count varlist))]
    (loop [[var & varlist] varlist
           vals vals
           result []]
      (if var
        (let [{:keys [explode]} var
              [h t] (split-at (cond-> 1 explode (+ extra)) vals)
              vals (if explode
                     (mapv #(URLDecoder/decode %) h)
                     (let [vs (str/split (first h) #"\,")]
                       (if (< (count vs) 2)
                         (URLDecoder/decode (first vs))
                         (mapv #(URLDecoder/decode %) vs))))]
          (recur varlist t (conj result (assoc var :val vals))))
        result))))

(defn expand [{:keys [varlist operator] :as expression} expansion]
  (if operator
    (case operator
      (\+ \#)
      (let [expansion (case operator
                        \+ expansion
                        \# (subs expansion 1))
            [v & extra-vars :as varlist] varlist]
        (if-not extra-vars
          {(:varname v) (URLDecoder/decode expansion)}
          (into {} (map (fn [k p]
                          [(:varname k) (URLDecoder/decode p)])
                        varlist
                        (str/split expansion #",")))))

      \.
      (let [values (str/split expansion #"\.")]
        (into {}
              (map (fn [{:keys [varname explode]} v]
                     [varname (if explode values
                                  (let [vs (str/split v #"\,")]
                                    (if (< (count vs) 2)
                                      (first vs)
                                      vs)))])
                   varlist (drop (max 0 (- (count values) (count varlist))) values))))

      \/
      (let [varlist (distribute-values varlist (str/split expansion #"\/"))]
        (zipmap (map :varname varlist) (map :val varlist)))

      \;
      (let [pairs (str/split expansion #"\;")]
        (into {}
              (filter seq
                      (map (fn [{:keys [varname]} pair]
                             (let [[k v] (str/split pair #"\=")]
                               (when (= varname k)
                                 [varname (or v "")])))
                           varlist pairs))))

      (\? \&)
      (let [pairs (str/split expansion #"&")
            params (into {} (for [[k v] (map #(str/split % #"\=") pairs)]
                              [k v]))]
        (into {}
              (for [{:keys [varname]} varlist
                    :when (find params varname)
                    :let [v (or (get params varname) "")]]
                [varname (some-> v URLDecoder/decode)])))

      (throw (ex-info "Unsupported operator" {:operator operator})))

    ;; default
    (into {} (map (fn [k v] [(:varname k) (URLDecoder/decode v)])
                  varlist (str/split expansion #",")))))
