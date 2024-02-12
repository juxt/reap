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

;; Matching URIs --------------------------------

(defn variable-type [var-types varname]
  (or
   (when-let [t (get var-types (keyword varname))]
     [(keyword varname) t])
   (when-let [t (get var-types varname)]
     [varname t])
   (throw
    (ex-info
     (format "var-type not found for '%s'" varname)
     {:var-types var-types
      :varname varname}))))

(defn distribute-values
  "Return the varlist augmented with values. If there are the same
  number of values as vars in the varlist, then each var will be
  associated with a value. If there are any extra values, these will
  be given to any var that has an explode modifier."
  [varlist vals var-types]
  (when (> (count (filter :explode varlist)) 1)
    (throw (ex-info "Cannot have multiple vars that have explode modifier set" {:varlist varlist})))
  (let [extra (- (count vals) (count varlist))]
    (loop [[var & varlist] varlist
           vals vals
           result []]
      (if var
        (let [{:keys [varname explode]} var
              [h t] (split-at (cond-> 1 explode (+ extra)) vals)
              [varname-k var-type] (variable-type var-types varname)
              vals [varname-k
                    (if-not explode

                      (let [vs (str/split (or (first h) "") #"\,")]
                        (case var-type
                          :string (URLDecoder/decode (first vs))
                          :integer (Long/parseLong (URLDecoder/decode (first vs)))
                          :list (mapv #(URLDecoder/decode %) vs)
                          :map (into {} (for [[k v] (partition 2 vs)]
                                          [k (URLDecoder/decode v)]))
                          :empty ""))

                      (case var-type
                        :string (str/join "/" h)
                        :integer (str/join "/" h)
                        :list (mapv #(URLDecoder/decode %) h)
                        :map (into {} (for [el h]
                                        (let [[k v] (str/split el #"=")]
                                          [k (URLDecoder/decode v)])
                                        ))
                        :empty ""))]]
          (recur varlist t (conj result vals)))
        result))))

(defn expand [{:keys [varlist operator] :as expression} var-types expansion]
  (let [varlist
        ;; We first filter the varlist to only include varnames that
        ;; we have type definitions for.
        (filter #(or
                  (contains? var-types (:varname %))
                  (contains? var-types (keyword (:varname %)))
                  (contains? var-types (str (:varname %)))
                  ) varlist)
        variable-type (fn [varname] (variable-type var-types varname))]
    (if operator
      (case operator
        (\+ \#)
        (let [expansion
              (cond
                (= operator \+) expansion
                (and (= operator \#) (str/starts-with? expansion "#"))
                (subs expansion 1)
                :else expansion)
              [v & extra-vars :as varlist] varlist]

          (if-not extra-vars
            (if v
              (let [{:keys [varname explode]} v
                    [varname-k var-type] (variable-type varname)]
                {varname-k
                 (case var-type
                   :integer (Long/parseLong (URLDecoder/decode expansion))
                   :list (mapv #(URLDecoder/decode %) (str/split expansion #","))
                   :map (into {}
                              (if explode
                                (for [pair (str/split expansion #",")]
                                  (let [[k v] (str/split pair #"=")]
                                    [k (URLDecoder/decode v)]))
                                (for [[k v] (partition 2 (str/split expansion #","))]
                                  [k (URLDecoder/decode v)])))
                   (URLDecoder/decode expansion))})
              {})
            (into {} (map (fn [k p]
                            (let [[varname-k var-type] (variable-type (:varname k))]
                              [varname-k
                               (let [dv (URLDecoder/decode p)]
                                 (case var-type
                                   :integer (Long/parseLong dv)
                                   dv)
                                 )]))
                          varlist
                          (str/split expansion #",")))))

        \.
        (let [values (next (str/split expansion #"\."))]
          (into {}
                (map (fn [{:keys [varname explode]} value]
                       (let [[varname-k var-type] (variable-type varname)]
                         [varname-k
                          (if explode
                            (case var-type
                              :string (URLDecoder/decode (first values))
                              :integer (Long/parseLong (URLDecoder/decode (first values)))
                              :list (mapv #(URLDecoder/decode %) values)
                              :map (into {}
                                         (for [[k v] (map #(str/split % #"=") values)]
                                           [k (URLDecoder/decode v)])))
                            (case var-type
                              :string (URLDecoder/decode value)
                              :integer (Long/parseLong (URLDecoder/decode value))
                              :list (if value
                                      (mapv #(URLDecoder/decode %) (str/split value #"\,"))
                                      [])
                              :map (into {}
                                         (for [[k v] (partition 2 (str/split value #","))]
                                           [k (URLDecoder/decode v)]))
                              ))]))
                     varlist
                     (concat values (repeat nil)))))

        \/
        (into {} (distribute-values varlist (str/split expansion #"\/") var-types))

        (\; \? \&)
        (let [pairs (str/split expansion (case operator \; #";" (\? \&) #"&"))
              params (reduce
                      (fn [acc pair]
                        (let [[k v] (str/split pair #"\=")]
                          (update acc k (fnil conj [])
                                  (or v ""))))
                      {} pairs)]
          (reduce
           (fn [acc {:keys [varname explode]}]
             (let [val (get params varname)
                   [varname-k var-type] (variable-type varname)]
               (assoc acc varname-k
                      (if-not explode
                        (case var-type
                          :string (URLDecoder/decode (or (first val) ""))
                          :integer (Long/parseLong (URLDecoder/decode (first val)))
                          :empty ""
                          :list (mapv #(URLDecoder/decode %) (str/split (first val) #","))
                          :map (into {} (for [[k v] (partition 2 (str/split (first val) #","))]
                                          [k (URLDecoder/decode v)]
                                          )))
                        ;; explode
                        (case var-type
                          :string (URLDecoder/decode (or (first val) ""))
                          :integer (Long/parseLong (URLDecoder/decode (first val)))
                          :empty ""
                          :list (mapv #(URLDecoder/decode %) val)
                          :map (into {} (for [[k v] params] [k (URLDecoder/decode (first v))]))
                          )))))
           {}
           varlist))

        (throw (ex-info "Unsupported operator" {:operator operator})))

      ;; Default here is Simple String Expansion: {var}
      (->> (str/split
            expansion
            #","
            ;; We pass -1 as explained in clojure.string/split to return
            ;; any trailing empty strings, which might be required to
            ;; associate with empty variables.
            -1)
           (map (fn [{:keys [varname explode]} v]
                  (let [dv (URLDecoder/decode v)
                        [varname-k var-type] (variable-type varname)]
                    [varname-k
                     (case var-type
                       :integer (Long/parseLong dv)
                       :list (str/split expansion #",")
                       :map (into {}
                                  (if explode
                                    (for [pair (str/split expansion #",")]
                                      (let [[k v] (str/split pair #"=")]
                                        [k (URLDecoder/decode v)]))
                                    (for [[k v] (partition 2 (str/split expansion #","))]
                                      [k (URLDecoder/decode v)])))
                       dv)]))
                varlist)
           (into {})))))
