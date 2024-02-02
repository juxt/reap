;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.rfc6570-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.rfc6570 :refer [make-uri match-uri compile-uri-template]]))

(deftest make-uri-test
  (are [uri-template variables expected]
      (=
       expected
       (make-uri
        (compile-uri-template uri-template)
        variables))

    "http://example.com/~{username}/"
    {"username" "fred"}
    "http://example.com/~fred/"

    "http://example.com/~{username:2}/"
    {"username" "fred"}
    "http://example.com/~fr/"

    "http://example.com/dictionary/{term:1}/{term}"
    {"term" "cat"}
    "http://example.com/dictionary/c/cat"

    "http://example.com/dictionary/{term:1}/{term}"
    {"term" "dog"}
    "http://example.com/dictionary/d/dog"

    "http://example.com/search{?q,lang}"
    {"q" "cat" "lang" "en"}
    "http://example.com/search?q=cat&lang=en"

    "http://example.com/search{?q,lang}"
    {"q" "chien" "lang" "fr"}
    "http://example.com/search?q=chien&lang=fr"

    "http://www.example.com/foo{?query,number}"
    {"query" "mycelium" "number" 100}
    "http://www.example.com/foo?query=mycelium&number=100"

    ;; "Alternatively, if 'query' is undefined, then the expansion would be..."

    "http://www.example.com/foo{?query,number}"
    {"number" 100}
    "http://www.example.com/foo?number=100"

    ;; "or if both variables are undefined, then it would be"

    "http://www.example.com/foo{?query,number}"
    {}
    "http://www.example.com/foo")

  ;; Level 1 examples

  (let [variables {"var" "value"
                   "hello" "Hello World!"}]

    (are [uri-template expected]
        (=
         expected
         (make-uri
          (compile-uri-template uri-template)
          variables))
      "{var}" "value"
      "{hello}" "Hello%20World%21"))

  ;; Level 2 examples

  (let [variables {"var" "value"
                   "hello" "Hello World!"
                   "path" "/foo/bar"}]
    (are [uri-template expected]
        (=
         expected
         (make-uri
          (compile-uri-template uri-template)
          variables))

      "{+var}" "value"
      "{+hello}" "Hello%20World!"
      "{+path}/here" "/foo/bar/here"
      "here?ref={+path}" "here?ref=/foo/bar"

      "X{#var}" "X#value"
      "X{#hello}" "X#Hello%20World!"))

  ;; Level 3 examples

  (let [variables {"var" "value"
                   "hello" "Hello World!"
                   "empty" ""
                   "path" "/foo/bar"
                   "x" "1024"
                   "y" "768"}]
    (are [uri-template expected]
        (=
         expected
         (make-uri
          (compile-uri-template uri-template)
          variables))

      "map?{x,y}" "map?1024,768"
      "{x,hello,y}" "1024,Hello%20World%21,768"

      "{+x,hello,y}" "1024,Hello%20World!,768"
      "{+path,x}/here" "/foo/bar,1024/here"

      "{#x,hello,y}" "#1024,Hello%20World!,768"
      "{#path,x}/here" "#/foo/bar,1024/here"

      "X{.var}" "X.value"
      "X{.x,y}" "X.1024.768"

      "{/var}" "/value"
      "{/var,x}/here" "/value/1024/here"

      "{;x,y}" ";x=1024;y=768"
      "{;x,y,empty}" ";x=1024;y=768;empty"

      "{?x,y}" "?x=1024&y=768"
      "{?x,y,empty}" "?x=1024&y=768&empty="

      "?fixed=yes{&x}" "?fixed=yes&x=1024"
      "{&x,y,empty}" "&x=1024&y=768&empty="

      ))

  ;; Level 4 examples

  (let [variables {"var" "value"
                   "hello" "Hello World!"
                   "path" "/foo/bar"
                   "list" '("red" "green" "blue")
                   "keys" {"semi" ";" "dot" "." "comma" ","}}]

    (are [uri-template expected]
        (=
         expected
         (make-uri
          (compile-uri-template uri-template)
          variables))

      ;; String expansion with value modifiers
      "{var:3}" "val"
      "{var:30}" "value"
      "{list}" "red,green,blue"
      "{list*}" "red,green,blue"
      "{keys}" "semi,%3B,dot,.,comma,%2C"
      "{keys*}" "semi=%3B,dot=.,comma=%2C"

      ;; Reserved expansion with value modifiers       (Sec 3.2.3)

      "{+path:6}/here" "/foo/b/here"
      "{+list}" "red,green,blue"
      "{+list*}" "red,green,blue"
      "{+keys}" "semi,;,dot,.,comma,,"
      "{+keys*}" "semi=;,dot=.,comma=,"

      ;; Fragment expansion with value modifiers       (Sec 3.2.4)

      "{#path:6}/here" "#/foo/b/here"
      "{#list}" "#red,green,blue"
      "{#list*}" "#red,green,blue"
      "{#keys}" "#semi,;,dot,.,comma,,"
      "{#keys*}" "#semi=;,dot=.,comma=,"

      ;; Label expansion, dot-prefixed                 (Sec 3.2.5)

      "X{.var:3}" "X.val"
      "X{.list}" "X.red,green,blue"
      "X{.list*}" "X.red.green.blue"
      "X{.keys}" "X.semi,%3B,dot,.,comma,%2C"
      "X{.keys*}" "X.semi=%3B.dot=..comma=%2C"

      ;; Path segments, slash-prefixed                 (Sec 3.2.6)

      "{/var:1,var}" "/v/value"
      "{/list}" "/red,green,blue"
      "{/list*}" "/red/green/blue"
      "{/list*,path:4}" "/red/green/blue/%2Ffoo"
      "{/keys}" "/semi,%3B,dot,.,comma,%2C"
      "{/keys*}" "/semi=%3B/dot=./comma=%2C"

      ;; Path-style parameters, semicolon-prefixed     (Sec 3.2.7)

      "{;hello:5}" ";hello=Hello"
      "{;list}" ";list=red,green,blue"
      "{;list*}" ";list=red;list=green;list=blue"
      "{;keys}" ";keys=semi,%3B,dot,.,comma,%2C"
      "{;keys*}" ";semi=%3B;dot=.;comma=%2C"

      ;; Form-style query, ampersand-separated         (Sec 3.2.8)

      "{?var:3}" "?var=val"
      "{?list}" "?list=red,green,blue"
      "{?list*}" "?list=red&list=green&list=blue"
      "{?keys}" "?keys=semi,%3B,dot,.,comma,%2C"
      "{?keys*}" "?semi=%3B&dot=.&comma=%2C"

      ;; Form-style query continuation                 (Sec 3.2.9)

      "{&var:3}" "&var=val"
      "{&list}" "&list=red,green,blue"
      "{&list*}" "&list=red&list=green&list=blue"
      "{&keys}" "&keys=semi,%3B,dot,.,comma,%2C"
      "{&keys*}" "&semi=%3B&dot=.&comma=%2C"

      )))

;; See RFC 6570 Section 3 (Expansion)
(deftest variable-expansion-test
  (let [variables
        {"count" ["one" "two" "three"]
         "dom" ["example" "com"]
         "dub" "me/too"
         "hello" "Hello World!"
         "half" "50%"
         "var" "value"
         "who" "fred"
         "base" "http://example.com/home/"
         "path" "/foo/bar"
         "list" ["red", "green", "blue"]
         "keys" {"semi" ";" "dot" "." "comma" ","}
         "v" "6"
         "x" "1024"
         "y" "768"
         "empty" ""
         "empty_keys" []
         "undef" nil}]

    ;; "A variable that is undefined (Section 2.3) has no value and is
    ;; ignored by the expansion process.  If all of the variables in an
    ;; expression are undefined, then the expression's expansion is the
    ;; empty string."
    (is (= "" (make-uri (compile-uri-template "{a,b,c}") variables)))
    (is (= "" (make-uri (compile-uri-template "{?a,b,c}") variables)))
    (is (= "" (make-uri (compile-uri-template "{/a,b,c}") variables)))
    (is (= "" (make-uri (compile-uri-template "{+a") variables)))

    (is (= "one,two,three"
           (make-uri
            (compile-uri-template "{count}")
            variables)))

    (is (= "one,two,three"
           (make-uri
            (compile-uri-template "{count*}")
            variables)))

    (is (= "/one,two,three"
           (make-uri
            (compile-uri-template "{/count}")
            variables)))

    (is (= "/one/two/three"
           (make-uri
            (compile-uri-template "{/count*}")
            variables)))

    (is (= ";count=one,two,three"
           (make-uri
            (compile-uri-template "{;count}")
            variables)))

    (is (= ";count=one;count=two;count=three"
           (make-uri
            (compile-uri-template "{;count*}")
            variables)))

    (is (= "?count=one,two,three"
           (make-uri
            (compile-uri-template "{?count}")
            variables)))

    (is (= "?count=one&count=two&count=three"
           (make-uri
            (compile-uri-template "{?count*}")
            variables)))

    (is (= "&count=one&count=two&count=three"
           (make-uri
            (compile-uri-template "{&count*}")
            variables)))

    ;; 3.2.2.  Simple String Expansion: {var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))

        "{var}" "value"
        "{hello}" "Hello%20World%21"
        "{half}" "50%25"
        "O{empty}X" "OX"
        "O{undef}X" "OX"
        "{x,y}" "1024,768"
        "{x,hello,y}" "1024,Hello%20World%21,768"
        "?{x,empty}" "?1024,"
        "?{x,undef}" "?1024"
        "?{undef,y}" "?768"
        "{var:3}" "val"
        "{var:30}" "value"
        "{list}" "red,green,blue"
        "{list*}" "red,green,blue"
        "{keys}" "semi,%3B,dot,.,comma,%2C"
        "{keys*}" "semi=%3B,dot=.,comma=%2C")

    ;; 3.2.3.  Reserved Expansion: {+var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))

        "{+var}" "value"
        "{+hello}" "Hello%20World!"
        "{+half}" "50%25"

        "{base}index" "http%3A%2F%2Fexample.com%2Fhome%2Findex"
        "{+base}index" "http://example.com/home/index"
        "O{+empty}X" "OX"
        "O{+undef}X" "OX"

        "{+path}/here" "/foo/bar/here"
        "here?ref={+path}" "here?ref=/foo/bar"
        "up{+path}{var}/here" "up/foo/barvalue/here"
        "{+x,hello,y}" "1024,Hello%20World!,768"
        "{+path,x}/here" "/foo/bar,1024/here"

        "{+path:6}/here" "/foo/b/here"
        "{+list}" "red,green,blue"
        "{+list*}" "red,green,blue"
        "{+keys}" "semi,;,dot,.,comma,,"
        "{+keys*}" "semi=;,dot=.,comma=,"

        )

    ;; TODO: 3.2.4.  Fragment Expansion: {#var}
    ;; TODO: 3.2.5.  Label Expansion with Dot-Prefix: {.var}
    ;; TODO: 3.2.6.  Path Segment Expansion: {/var}
    ;; TODO: 3.2.7.  Path-Style Parameter Expansion: {;var}
    ;; TODO: 3.2.8.  Form-Style Query Expansion: {?var}
    ;; TODO: 3.2.9.  Form-Style Query Continuation: {&var}

    ))


(deftest match-uri-test
  (are [uri-template uri expected]
      (=
       expected
       (:vars
        (match-uri
         (compile-uri-template uri-template)
         uri)))

    ;; Preliminaries

      "http://example.com/~{username}/"
      "http://example.com/~mal/"
      {"username" "mal"}

      "http://example.com/~{username,id}/"
      "http://example.com/~mal,01/"
      {"username" "mal" "id" "01"}

      "http://example.com/dictionary/{term:1}/{term}"
      "http://example.com/dictionary/c/cat"
      {"term" "cat"}

      "http://example.com/search{?q,lang}"
      "http://example.com/search?q=chien&lang=fr"
      {"q" "chien" "lang" "fr"}

      "http://example.com/file{.suffix}"
      "http://example.com/file.svg"
      {"suffix" "svg"}

      ;; If there are multiple suffixes, we want the right most one
      "http://example.com/file{.suffix}"
      "http://example.com/file.svg.xml"
      {"suffix" "xml"}

      ;; If there are multiple suffixes, we want the right most one
      "http://example.com/file{.inner,outer}"
      "http://example.com/file.v.svg.xml"
      {"inner" "svg" "outer" "xml"}

      ;; Level 1 examples

      "{var}" "value" {"var" "value"}
      "{hello}" "Hello%20World%21" {"hello" "Hello World!"}

      ;; Level 2

      ;; Reserved string expansion (Sec 3.2.3)

      "{+var}" "value" {"var" "value"}
      "{+hello}" "Hello%20World!" {"hello" "Hello World!"}
      "{+path}/here" "/foo/bar/here" {"path" "/foo/bar"}
      "here?ref={+path}" "here?ref=/foo/bar" {"path" "/foo/bar"}

      ;; Fragment expansion, crosshatch-prefixed (Sec 3.2.4)

      "X{#var}" "X#value" {"var" "value"}
      "X{#hello}" "X#Hello%20World!" {"hello" "Hello World!"})


  ;; Level 3 examples
  (let [variables {"var" "value"
                   "hello" "Hello World!"
                   "empty" ""
                   "path" "/foo/bar"
                   "x" "1024"
                   "y" "768"}]

    (are [uri-template uri expected]
        (=
         expected
         (:vars
          (match-uri
           (compile-uri-template uri-template)
           uri)))

      ;; String expansion with multiple variables (Sec 3.2.2)

        "map?{x,y}" "map?1024,768"
        (select-keys variables ["x" "y"])

        "{x,hello,y}" "1024,Hello%20World%21,768"
        (select-keys variables ["x" "hello" "y"])

        ;; Reserved expansion with multiple variables (Sec 3.2.3)

        "{+x,hello,y}" "1024,Hello%20World!,768"
        (select-keys variables ["x" "hello" "y"])

        "{+path,x}/here" "/foo/bar,1024/here"
        (select-keys variables ["path" "x"])

        ;; Fragment expansion with multiple variables (Sec 3.2.4)

        "{#x,hello,y}" "#1024,Hello%20World!,768"
        (select-keys variables ["x" "hello" "y"])

        "{#path,x}/here" "#/foo/bar,1024/here"
        (select-keys variables ["path" "x"])

        ;; Label expansion, dot-prefixed (Sec 3.2.5)

        "X{.var}" "X.value"
        (select-keys variables ["var"])

        "X{.x,y}" "X.1024.768"
        (select-keys variables ["x" "y"])

        ;; Path segments, slash-prefixed (Sec 3.2.6)
        "{/var}" "/value"
        (select-keys variables ["var"])

        "{/var,x}/here" "/value/1024/here"
        (select-keys variables ["var" "x"])

        ;; Path-style parameters, semicolon-prefixed (Sec 3.2.7)

        "{;x,y}" ";x=1024;y=768"
        (select-keys variables ["x" "y"])

        "{;x,y,empty}" ";x=1024;y=768;empty"
        (select-keys variables ["x" "y" "empty"])

        ;; Form-style query, ampersand-separated (Sec 3.2.8)
        "{?x,y}"
        "?x=1024&y=768"
        (select-keys variables ["x" "y"])

        "{?x,y,empty}" "?x=1024&y=768&empty"
        (select-keys variables ["x" "y" "empty"])

        ;; Form-style query continuation (Sec 3.2.9)

        "?fixed=yes{&x}" "?fixed=yes&x=1024"
        (select-keys variables ["x"])

        "{&x,y,empty}" "&x=1024&y=768&empty="
        (select-keys variables ["x" "y" "empty"])))

  ;; Level 4
  (let [variables {"var" "value"
                   "hello" "Hello World!"
                   "path" "/foo/bar"
                   "list" ["red" "green" "blue"]
                   "keys" {"semi" ";" "dot" "." "comma" ","}}]
    (are [uri-template uri expected]
        (=
         expected
         (:vars
          (match-uri
           (compile-uri-template uri-template)
           uri)))

        "{/var:1,var}"
        "/v/value"
        (select-keys variables ["var"])

        "{/list}"
        "/red,green,blue"
        (select-keys variables ["list"])

        "{/list*}"
        "/red/green/blue"
        (select-keys variables ["list"])

        "{/list*,path:4}"
        "/red/green/blue/%2Ffoo"
        {"list" ["red" "green" "blue"]
         "path" "/foo"}

        "X{.list}"
        "X.red,green,blue"
        (select-keys variables ["list"])

        "X{.list*}"
        "X.red.green.blue"
        (select-keys variables ["list"])

        )))
