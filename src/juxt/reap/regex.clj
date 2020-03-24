;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.regex)

(def regex-chars
  (merge
   {(int \\) "\\\\"
    (int \u0009) "\\t"
    (int \u000A) "\\n"
    (int \u000D) "\\r"
    (int \u000C) "\\f"
    (int \u0007) "\\a"
    (int \u001B) "\\e"}
   (into {} (for [n (concat
                     (range (int \A) (inc (int \Z)))
                     (range (int \a) (inc (int \z)))
                     (range (int \0) (inc (int \9))))]
              [n (str (char n))]))))

(defn int->regex
  "Convert a number to a regex that can be used in a regular expression
  pattern. Characters with Unicode values of less than 256 are printed
  with the \\x00 style, whereas all other characters in the Basic
  Multilingual Plane are printed with the \\u0000 notation. Also
  supports characters beyond the BMP."
  [n]
  (cond (< n 256) (get regex-chars n (format "\\x%02X" n))
        (< n 65536) (format "\\u%04X" n)
        :else (format "\\x{%04X}" n)))

(defprotocol RegularExpressionPattern
  (re-str [_] "Return a string that represents the Java regex"))

(extend-protocol RegularExpressionPattern
  String
  (re-str [s] s)
  Character
  (re-str [c]
    (int->regex (int c)))
  Integer
  (re-str [n]
    (int->regex n))
  Long
  (re-str [n]
    (assert (<= n Integer/MAX_VALUE))
    (int->regex (int n)))
  java.util.regex.Pattern
  (re-str [re]
    (str re)))

(defn re-concat
  "Compose a regular expression string representing a sequence of the
  given arguments."
  [& args]
  (apply str (map re-str args)))

(defn re-compose [fmt & args]
  (apply format fmt (map re-str args)))

(defn group [& args]
  (str "(" (apply re-concat args) ")"))

(defn ^String zero-or-more [alts]
  (re-str (str "[" (apply str (map re-str alts)) "]*")))

(defn ^String one-or-more [alts]
  (re-str (str "[" (apply str (map re-str alts)) "]+")))

(defn ^String optional [alts]
  (re-str (str "[" (apply str (map re-str alts)) "]")))

(defn find-zero-or-more [m]
  (lazy-seq
   (loop []
     (when (re-find m)
       (cons (re-groups m) (find-zero-or-more m))))))

(defn input [input]
  (re-matcher #"" input))

(defn ^java.util.regex.Matcher advance-and-return
  [^java.util.regex.Matcher matcher res]
  (.region matcher (.end matcher) (.regionEnd matcher))
  res)

(defn re-find-with-pattern
  [^java.util.regex.Matcher m ^java.util.regex.Pattern re]
  (.usePattern m re)
  (re-find m))

(defn pattern-parser [pat]
  (fn [matcher]
    (.usePattern matcher pat)
    (when (.lookingAt matcher)
      (advance-and-return matcher (.group matcher 0)))))
