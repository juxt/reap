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

        )))

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

      "{var}"
      "value"
      {"var" "value"}

      "{hello}"
      "Hello%20World%21"
      {"hello" "Hello World!"}

      ;; Level 2

      ;; Reserved string expansion (Sec 3.2.3)

      "{+var}"
      "value"
      {"var" "value"}

      "{+hello}"
      "Hello%20World!"
      {"hello" "Hello World!"}

      "{+path}/here"
      "/foo/bar/here"
      {"path" "/foo/bar"}

      "here?ref={+path}"
      "here?ref=/foo/bar"
      {"path" "/foo/bar"}

      ;; Fragment expansion, crosshatch-prefixed (Sec 3.2.4)

      "X{#var}"
      "X#value"
      {"var" "value"}

      "X{#hello}"
      "X#Hello%20World!"
      {"hello" "Hello World!"}

      ;; Level 3

      ;; String expansion with multiple variables (Sec 3.2.2)

      "map?{x,y}"
      "map?1024,768"
      {"x" "1024" "y" "768"}

      "{x,hello,y}"
      "1024,Hello%20World%21,768"
      {"x" "1024" "hello" "Hello World!" "y" "768"}

      ;; Reserved expansion with multiple variables (Sec 3.2.3)

      "{+x,hello,y}"
      "1024,Hello%20World!,768"
      {"x" "1024" "hello" "Hello World!" "y" "768"}

      "{+path,x}/here"
      "/foo/bar,1024/here"
      {"path" "/foo/bar" "x" "1024"}

      ;; Fragment expansion with multiple variables (Sec 3.2.4)

      "{#x,hello,y}"
      "#1024,Hello%20World!,768"
      {"x" "1024" "hello" "Hello World!" "y" "768"}

      "{#path,x}/here"
      "#/foo/bar,1024/here"
      {"path" "/foo/bar" "x" "1024"}

      ;; Label expansion, dot-prefixed (Sec 3.2.5)

      "X{.var}"
      "X.value"
      {"var" "value"}

      "X{.x,y}"
      "X.1024.768"
      {"x" "1024" "y" "768"}

      ;; Path segments, slash-prefixed (Sec 3.2.6)
      "{/var}"
      "/value"
      {"var" "value"}

      "{/var,x}/here"
      "/value/1024/here"
      {"var" "value" "x" "1024"}

      ;; Path-style parameters, semicolon-prefixed (Sec 3.2.7)
      "{;x,y}"
      ";x=1024;y=768"
      {"x" "1024" "y" "768"}

      "{;x,y,empty}"
      ";x=1024;y=768;empty"
      {"x" "1024" "y" "768" "empty" nil}

      ;; Form-style query, ampersand-separated (Sec 3.2.8)
      "{?x,y}"
      "?x=1024&y=768"
      {"x" "1024" "y" "768"}

      "{?x,y,empty}"
      "?x=1024&y=768&empty"
      {"x" "1024" "y" "768" "empty" nil}

      ;; Form-style query continuation (Sec 3.2.9)
      ;; TODO

      ;; Level 4

      "{/var:1,var}"
      "/v/value"
      {"var" "value"}

      "{/list}"
      "/red,green,blue"
      {"list" ["red" "green" "blue"]}

      "{/list*}"
      "/red/green/blue"
      {"list" ["red" "green" "blue"]}

      "{/list*,path:4}"
      "/red/green/blue/%2Ffoo"
      {"list" ["red" "green" "blue"]
       "path" "/foo"}

      "X{.list}"
      "X.red,green,blue"
      {"list" ["red" "green" "blue"]}

      "X{.list*}"
      "X.red.green.blue"
      {"list" ["red" "green" "blue"]}))
