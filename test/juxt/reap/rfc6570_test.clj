;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.rfc6570-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [juxt.reap.rfc6570 :refer [make-uri match-uri compile-uri-template component->str var->str]]
   [clojure.string :as str]))

(deftest make-uri-test
  (are [uri-template variables expected]
      (=
       expected
       (make-uri
        (compile-uri-template uri-template)
        variables))

    "http://example.com/~{username}/"
    {:username "fred"}
    "http://example.com/~fred/"

    "http://example.com/~{username:2}/"
    {:username "fred"}
    "http://example.com/~fr/"

    "http://example.com/dictionary/{term:1}/{term}"
    {:term "cat"}
    "http://example.com/dictionary/c/cat"

    "http://example.com/dictionary/{term:1}/{term}"
    {:term "dog"}
    "http://example.com/dictionary/d/dog"

    "http://example.com/search{?q,lang}"
    {:q "cat" :lang "en"}
    "http://example.com/search?q=cat&lang=en"

    "http://example.com/search{?q,lang}"
    {:q "chien" :lang "fr"}
    "http://example.com/search?q=chien&lang=fr"

    "http://www.example.com/foo{?query,number}"
    {:query "mycelium" :number 100}
    "http://www.example.com/foo?query=mycelium&number=100"

    ;; "Alternatively, if 'query' is undefined, then the expansion would be..."

    "http://www.example.com/foo{?query,number}"
    {:number 100}
    "http://www.example.com/foo?number=100"

    ;; "or if both variables are undefined, then it would be"

    "http://www.example.com/foo{?query,number}"
    {}
    "http://www.example.com/foo")

  ;; Level 1 examples

  (let [variables {:var "value"
                   :hello "Hello World!"}]

    (are [uri-template expected]
        (=
         expected
         (make-uri
          (compile-uri-template uri-template)
          variables))
      "{var}" "value"
      "{hello}" "Hello%20World%21"))

  ;; Level 2 examples

  (let [variables {:var "value"
                   :hello "Hello World!"
                   :path "/foo/bar"}]
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

  (let [variables {:var "value"
                   :hello "Hello World!"
                   :empty ""
                   :path "/foo/bar"
                   :x "1024"
                   :y "768"}]
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

  (let [variables {:var "value"
                   :hello "Hello World!"
                   :path "/foo/bar"
                   :list '("red" "green" "blue")
                   :keys {"semi" ";" "dot" "." "comma" ","}}]

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
        {:count ["one" "two" "three"]
         :dom ["example" "com"]
         :dub "me/too"
         :hello "Hello World!"
         :half "50%"
         :var "value"
         :who "fred"
         :base "http://example.com/home/"
         :path "/foo/bar"
         :list ["red", "green", "blue"]
         :keys {"semi" ";" "dot" "." "comma" ","}
         :v 6
         :x 1024
         :y 768
         :empty ""
         "empty_keys" []
         :undef nil}]

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
        "{+keys*}" "semi=;,dot=.,comma=,")

    ;; 3.2.4.  Fragment Expansion: {#var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))

        "{#var}" "#value"
        "{#hello}" "#Hello%20World!"
        "{#half}" "#50%25"
        "foo{#empty}" "foo#"
        "foo{#undef}" "foo"
        "{#x,hello,y}" "#1024,Hello%20World!,768"
        "{#path,x}/here" "#/foo/bar,1024/here"
        "{#path:6}/here" "#/foo/b/here"
        "{#list}" "#red,green,blue"
        "{#list*}" "#red,green,blue"
        "{#keys}" "#semi,;,dot,.,comma,,"
        "{#keys*}" "#semi=;,dot=.,comma=,")

    ;; 3.2.5.  Label Expansion with Dot-Prefix: {.var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))

        "{.who}" ".fred"
        "{.who,who}" ".fred.fred"
        "{.half,who}" ".50%25.fred"
        "www{.dom*}" "www.example.com"
        "X{.var}" "X.value"
        "X{.empty}" "X."
        "X{.undef}" "X"
        "X{.var:3}" "X.val"
        "X{.list}" "X.red,green,blue"
        "X{.list*}" "X.red.green.blue"
        "X{.keys}" "X.semi,%3B,dot,.,comma,%2C"
        "X{.keys*}" "X.semi=%3B.dot=..comma=%2C"
        "X{.empty_keys}" "X"
        "X{.empty_keys*}" "X")

    ;; 3.2.6.  Path Segment Expansion: {/var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))

        "{/who}"             "/fred"
        "{/who,who}"         "/fred/fred"
        "{/half,who}"        "/50%25/fred"
        "{/who,dub}"         "/fred/me%2Ftoo"
        "{/var}"             "/value"
        "{/var,empty}"       "/value/"
        "{/var,undef}"       "/value"
        "{/var,x}/here"      "/value/1024/here"
        "{/var:1,var}"       "/v/value"
        "{/list}"            "/red,green,blue"
        "{/list*}"           "/red/green/blue"
        "{/list*,path:4}"    "/red/green/blue/%2Ffoo"
        "{/keys}"            "/semi,%3B,dot,.,comma,%2C"
        "{/keys*}"           "/semi=%3B/dot=./comma=%2C")

    ;; 3.2.7.  Path-Style Parameter Expansion: {;var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))
        "{;who}" ";who=fred"
        "{;half}" ";half=50%25"
        "{;empty}" ";empty"
        "{;v,empty,who}" ";v=6;empty;who=fred"
        "{;v,bar,who}" ";v=6;who=fred"
        "{;x,y}" ";x=1024;y=768"
        "{;x,y,empty}" ";x=1024;y=768;empty"
        "{;x,y,undef}" ";x=1024;y=768"
        "{;hello:5}" ";hello=Hello"
        "{;list}" ";list=red,green,blue"
        "{;list*}" ";list=red;list=green;list=blue"
        "{;keys}" ";keys=semi,%3B,dot,.,comma,%2C"
        "{;keys*}" ";semi=%3B;dot=.;comma=%2C")

    ;; 3.2.8.  Form-Style Query Expansion: {?var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))

        "{?who}"             "?who=fred"
        "{?half}"            "?half=50%25"
        "{?x,y}"             "?x=1024&y=768"
        "{?x,y,empty}"       "?x=1024&y=768&empty="
        "{?x,y,undef}"       "?x=1024&y=768"
        "{?var:3}"           "?var=val"
        "{?list}"            "?list=red,green,blue"
        "{?list*}"           "?list=red&list=green&list=blue"
        "{?keys}"            "?keys=semi,%3B,dot,.,comma,%2C"
        "{?keys*}"           "?semi=%3B&dot=.&comma=%2C")

    ;; 3.2.9.  Form-Style Query Continuation: {&var}

    (are [template expansion]
        (= expansion (make-uri (compile-uri-template template) variables))

        "{&who}" "&who=fred"
        "{&half}" "&half=50%25"
        "?fixed=yes{&x}" "?fixed=yes&x=1024"
        "{&x,y,empty}" "&x=1024&y=768&empty="
        "{&x,y,undef}" "&x=1024&y=768"
        "{&var:3}" "&var=val"
        "{&list}" "&list=red,green,blue"
        "{&list*}" "&list=red&list=green&list=blue"
        "{&keys}" "&keys=semi,%3B,dot,.,comma,%2C"
        "{&keys*}" "&semi=%3B&dot=.&comma=%2C")))

;; Section 2.4.2
(deftest composite-values-test
  (is (=
       "/mapper?city=Newport%20Beach&state=CA"
       (make-uri
        (compile-uri-template "/mapper{?address*}")
        {"address" {"city" "Newport Beach"
                    "state" "CA"}})))

  (is (= {"address" {"city" "Newport Beach" "state" "CA"}}
         (match-uri
          (compile-uri-template "/mapper{?address*}")
          {"address" :map}
          "/mapper?city=Newport%20Beach&state=CA")))

  (let [variables {"year" ["1965" "2000" "2012"]
                   "dom" ["example" "com"]}]
    (let [uri-template (compile-uri-template "find{?year*}")]
      (is (= "find?year=1965&year=2000&year=2012"
             (make-uri uri-template variables)))

      (is (= {"year" ["1965" "2000" "2012"]}
             (match-uri
              uri-template
              {"year" :list}
              "find?year=1965&year=2000&year=2012"))))

    (let [uri-template (compile-uri-template "www{.dom*}")]
      (is (= "www.example.com"
             (make-uri uri-template variables)))

      (is (= {"dom" ["example" "com"]}
             (match-uri
              uri-template
              {"dom" :list}
              "www.example.com"))))))

(deftest match-uri-test
  (let [variables
        {:count ["one" "two" "three"]
         :dom ["example" "com"]
         :dub "me/too"
         :hello "Hello World!"
         :half "50%"
         :var "value"
         :who "fred"
         :base "http://example.com/home/"
         :path "/foo/bar"
         :list ["red", "green", "blue"]
         :keys {"semi" ";" "dot" "." "comma" ","}
         :v 6
         :x 1024
         :y 768
         :empty ""
         "empty_keys" []
         :undef nil}
        var-types
        {"count" :list
         :dom :list
         :dub :string
         :hello :string
         :half :string
         :var :string
         :who :string
         :base :string
         :path :string
         :list :list
         :keys :map
         :v :integer
         :x :integer
         :y :integer
         :empty :empty
         "empty_keys" :list}]

    (testing "Simple String Expansion: {var}"

      (is
       (=
        {:var (:var variables)}
        (match-uri
         (compile-uri-template "{var}")
         var-types
         "value")))

      (is
       (=
        {:hello (:hello variables)}
        (match-uri
         (compile-uri-template "{hello}")
         var-types
         "Hello%20World%21")))

      (is
       (=
        {:half "50%"}
        (match-uri
         (compile-uri-template "{half}")
         var-types
         "50%25")))

      (is
       (=
        {:empty ""}
        (match-uri
         (compile-uri-template "O{empty}X")
         var-types
         "OX")))

      (is
       (=
        {}
        (match-uri
         (compile-uri-template "O{undef}X")
         var-types
         "OX")))

      (is
       (=
        {:x 1024 :y 768}
        (match-uri
         (compile-uri-template "{x,y}")
         var-types
         "1024,768")))

      (is
       (=
        {:x 1024 :hello "Hello World!" :y 768}
        (match-uri
         (compile-uri-template "{x,hello,y}")
         var-types
         "1024,Hello%20World%21,768")))

      (is
       (=
        {:x 1024, :empty ""}
        (match-uri
         (compile-uri-template "?{x,empty}")
         var-types
         "?1024,")))

      (is
       (=
        {:x 1024}
        (match-uri
         (compile-uri-template "?{x,undef}")
         var-types
         "?1024")))

      (is
       (=
        {:y 768}
        (match-uri
         (compile-uri-template "?{undef,y}")
         var-types
         "?768")))

      (is
       (=
        {:var "val"}
        (match-uri
         (compile-uri-template "{var:3}")
         var-types
         "val")))

      (is
       (=
        {:var "value"}
        (match-uri
         (compile-uri-template "{var:30}")
         var-types
         "value")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{list}")
         var-types
         "red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{list*}")
         var-types
         "red,green,blue")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{keys}")
         var-types
         "semi,%3B,dot,.,comma,%2C")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{keys*}")
         var-types
         "semi=%3B,dot=.,comma=%2C"))))

    (testing "Reserved Expansion: {+var}"
      (is
       (=
        (select-keys variables [:var])
        (match-uri
         (compile-uri-template "{+var}")
         var-types
         "value")))

      (is
       (=
        (select-keys variables [:hello])
        (match-uri
         (compile-uri-template "{+hello}")
         var-types
         "Hello%20World!")))

      (is
       (=
        (select-keys variables [:hello])
        (match-uri
         (compile-uri-template "{+hello}")
         var-types
         "Hello%20World!")))

      (is
       (=
        (select-keys variables [:half])
        (match-uri
         (compile-uri-template "{+half}")
         var-types
         "50%25")))

      (is
       (=
        (select-keys variables [:base])
        (match-uri
         (compile-uri-template "{base}index")
         var-types
         "http%3A%2F%2Fexample.com%2Fhome%2Findex")))

      (is
       (=
        (select-keys variables [:base])
        (match-uri
         (compile-uri-template "{+base}index")
         var-types
         "http://example.com/home/index")))

      (is
       (=
        (select-keys variables [:empty])
        (match-uri
         (compile-uri-template "O{+empty}X")
         var-types
         "OX")))

      (is
       (=
        {}
        (match-uri
         (compile-uri-template "O{+undef}X")
         var-types
         "OX")))

      (is
       (=
        (select-keys variables [:path])
        (match-uri
         (compile-uri-template "{+path}/here")
         var-types
         "/foo/bar/here")))

      (is
       (=
        (select-keys variables [:path])
        (match-uri
         (compile-uri-template "here?ref={+path}")
         var-types
         "here?ref=/foo/bar")))

      ;; This is possible to make but not to match
      #_(is
         (=
          (select-keys variables ["path" "var"])
          (match-uri
           (compile-uri-template "up{+path}{var}/here")
           var-types
           "up/foo/barvalue/here")))

      (is
       (=
        (select-keys variables [:x :y :hello])
        (match-uri
         (compile-uri-template "{+x,hello,y}")
         var-types
         "1024,Hello%20World!,768")))

      (is
       (=
        (select-keys variables [:path :x])
        (match-uri
         (compile-uri-template "{+path,x}/here")
         var-types
         "/foo/bar,1024/here")))

      (is
       (=
        {:path "/foo/b"}
        (match-uri
         (compile-uri-template "{+path:6}/here")
         var-types
         "/foo/b/here")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{+list}")
         var-types
         "red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{+list*}")
         var-types
         "red,green,blue")))

      (is
       (=
        {:keys {"semi" ";", "dot" "."}}
        (match-uri
         (compile-uri-template "{+keys}")
         var-types
         "semi,;,dot,.")))

      ;; Comma can't be supported due to parsing ambiguity
      (is
       (=
        {:keys {"semi" ";", "dot" "."}}
        (match-uri
         (compile-uri-template "{+keys*}")
         var-types
         "semi=;,dot=."))))

    (testing "Fragment Expansion: {#var}"

      (is
       (=
        (select-keys variables [:var])
        (match-uri
         (compile-uri-template "{#var}")
         var-types
         "#value")))

      (is
       (=
        (select-keys variables [:hello])
        (match-uri
         (compile-uri-template "{#hello}")
         var-types
         "#Hello%20World!")))

      (is
       (=
        (select-keys variables [:half])
        (match-uri
         (compile-uri-template "{#half}")
         var-types
         "#50%25")))

      (is
       (=
        (select-keys variables [:empty])
        (match-uri
         (compile-uri-template "foo{#empty}")
         var-types
         "foo#")))

      (is
       (=
        {}
        (match-uri
         (compile-uri-template "foo{#undef}")
         var-types
         "foo")))

      (is
       (=
        (select-keys variables [:x :hello :y])
        (match-uri
         (compile-uri-template "{#x,hello,y}")
         var-types
         "#1024,Hello%20World!,768")))

      (is
       (=
        (select-keys variables [:path :x])
        (match-uri
         (compile-uri-template "{#path,x}/here")
         var-types
         "#/foo/bar,1024/here")))

      (is
       (=
        (update (select-keys variables [:path]) :path subs 0 6)
        (match-uri
         (compile-uri-template "{#path:6}/here")
         var-types
         "#/foo/b/here")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{#list}")
         var-types
         "#red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{#list*}")
         var-types
         "#red,green,blue")))

      ;; We must remove the comma because we can't distinguish it
      (is
       (=
        (update (select-keys variables [:keys]) :keys select-keys ["semi" "dot"])
        (match-uri
         (compile-uri-template "{#keys}")
         var-types
         "#semi,;,dot,.")))

      ;; We must remove the comma because we can't distinguish it
      (is
       (=
        (update (select-keys variables [:keys]) :keys select-keys ["semi" "dot"])
        (match-uri
         (compile-uri-template "{#keys*}")
         var-types
         "#semi=;,dot=."))))

    (testing "Label Expansion with Dot-Prefix: {.var}"

      (is
       (=
        (select-keys variables [:who])
        (match-uri
         (compile-uri-template "{.who}")
         var-types
         ".fred")))

      (is
       (=
        (select-keys variables [:who])
        (match-uri
         (compile-uri-template "{.who,who}")
         var-types
         ".fred.fred")))

      (is
       (=
        (select-keys variables [:half :who])
        (match-uri
         (compile-uri-template "{.half,who}")
         var-types
         ".50%25.fred")))

      (is
       (=
        (select-keys variables [:dom])
        (match-uri
         (compile-uri-template "www{.dom*}")
         var-types
         "www.example.com")))

      (is
       (=
        (select-keys variables [:var])
        (match-uri
         (compile-uri-template "X{.var}")
         var-types
         "X.value")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "X{.list}")
         var-types
         "X.red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "X{.list*}")
         var-types
         "X.red.green.blue")))

      (is
       (=
        (-> (select-keys variables [:keys])
            ;; We remove the "dot" entry because it's ambiguous to
            ;; parse out
            (update :keys dissoc "dot"))
        (match-uri
         (compile-uri-template "X{.keys}")
         var-types
         "X.semi,%3B,comma,%2C")))

      (is
       (=
        (-> (select-keys variables [:keys])
            ;; We remove the "dot" entry because it's ambiguous to
            ;; parse out
            (update :keys dissoc "dot"))
        (match-uri
         (compile-uri-template "X{.keys*}")
         var-types
         "X.semi=%3B.comma=%2C")))

      (is
       (=
        (select-keys variables ["empty_keys"])
        (match-uri
         (compile-uri-template "X{.empty_keys}")
         var-types
         "X")))

      (is
       (=
        (select-keys variables ["empty_keys"])
        (match-uri
         (compile-uri-template "X{.empty_keys*}")
         var-types
         "X"))))

    (testing "Path Segment Expansion: {/var}"

      (is
       (=
        (select-keys variables [:who])
        (match-uri
         (compile-uri-template "{/who}")
         var-types
         "/fred")))

      (is
       (=
        (select-keys variables [:who])
        (match-uri
         (compile-uri-template "{/who,who}")
         var-types
         "/fred/fred")))

      (is
       (=
        (select-keys variables [:half :who])
        (match-uri
         (compile-uri-template "{/half,who}")
         var-types
         "/50%25/fred")))

      (is
       (=
        (select-keys variables [:who :dub])
        (match-uri
         (compile-uri-template "{/who,dub}")
         var-types
         "/fred/me%2Ftoo")))

      (is
       (=
        (select-keys variables [:var])
        (match-uri
         (compile-uri-template "{/var}")
         var-types
         "/value")))

      (is
       (=
        (select-keys variables [:var :empty])
        (match-uri
         (compile-uri-template "{/var,empty}")
         var-types
         "/value/")))

      (is
       (=
        (select-keys variables [:var])
        (match-uri
         (compile-uri-template "{/var,undef}")
         var-types
         "/value")))

      (is
       (=
        (select-keys variables [:var :x])
        (match-uri
         (compile-uri-template "{/var,x}/here")
         var-types
         "/value/1024/here")))

      (is
       (=
        (select-keys variables [:var])
        (match-uri
         (compile-uri-template "{/var:1,var}")
         var-types
         "/v/value")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{/list}")
         var-types
         "/red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{/list*}")
         var-types
         "/red/green/blue")))

      (is
       (=
        (->
         (select-keys variables [:list :path])
         (update :path subs 0 4))
        (match-uri
         (compile-uri-template "{/list*,path:4}")
         var-types
         "/red/green/blue/%2Ffoo")))

      (testing "keys"
        (is
         (=
          (select-keys variables [:keys])
          (match-uri
           (compile-uri-template "{/keys}")
           var-types
           "/semi,%3B,dot,.,comma,%2C"))))

      (testing "keys explode"
        (is
         (=
          (select-keys variables [:keys])
          (match-uri
           (compile-uri-template "{/keys*}")
           var-types
           "/semi=%3B/dot=./comma=%2C")))))

    (testing "Path-Style Parameter Expansion: {;var}"

      (is
       (=
        (select-keys variables [:who])
        (match-uri
         (compile-uri-template "{;who}")
         var-types
         ";who=fred")))

      (is
       (=
        (select-keys variables [:half])
        (match-uri
         (compile-uri-template "{;half}")
         var-types
         ";half=50%25")))

      (is
       (=
        (select-keys variables [:empty])
        (match-uri
         (compile-uri-template "{;empty}")
         var-types
         ";empty")))

      (is
       (=
        (select-keys variables [:v :empty :who])
        (match-uri
         (compile-uri-template "{;v,empty,who}")
         var-types
         ";v=6;empty;who=fred")))

      (is
       (=
        (select-keys variables [:v :bar :who])
        (match-uri
         (compile-uri-template "{;v,bar,who}")
         var-types
         ";v=6;who=fred")))

      (is
       (=
        (select-keys variables [:x :y])
        (match-uri
         (compile-uri-template "{;x,y}")
         var-types
         ";x=1024;y=768")))

      (is
       (=
        (select-keys variables [:x :y :empty])
        (match-uri
         (compile-uri-template "{;x,y,empty}")
         var-types
         ";x=1024;y=768;empty")))

      (is
       (=
        (select-keys variables [:x :y])
        (match-uri
         (compile-uri-template "{;x,y,undef}")
         var-types
         ";x=1024;y=768")))

      (is
       (=
        (update (select-keys variables [:hello]) :hello subs 0 5)
        (match-uri
         (compile-uri-template "{;hello:5}")
         var-types
         ";hello=Hello")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{;list}")
         var-types
         ";list=red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{;list*}")
         var-types
         ";list=red;list=green;list=blue")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{;keys}")
         var-types
         ";keys=semi,%3B,dot,.,comma,%2C")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{;keys*}")
         var-types
         ";semi=%3B;dot=.;comma=%2C"))))

    (testing "Form-Style Query Expansion: {?var}"

      (is
       (=
        (select-keys variables [:who])
        (match-uri
         (compile-uri-template "{?who}")
         var-types
         "?who=fred")))

      (is
       (=
        (select-keys variables [:half])
        (match-uri
         (compile-uri-template "{?half}")
         var-types
         "?half=50%25")))

      (is
       (=
        (select-keys variables [:x :y])
        (match-uri
         (compile-uri-template "{?x,y}")
         var-types
         "?x=1024&y=768")))

      (is
       (=
        (select-keys variables [:x :y :empty])
        (match-uri
         (compile-uri-template "{?x,y,empty}")
         var-types
         "?x=1024&y=768&empty=")))

      (is
       (=
        (select-keys variables [:x :y])
        (match-uri
         (compile-uri-template "{?x,y,undef}")
         var-types
         "?x=1024&y=768")))

      (is
       (=
        (-> (select-keys variables [:var])
            (update :var subs 0 3))
        (match-uri
         (compile-uri-template "{?var:3}")
         var-types
         "?var=val")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{?list}")
         var-types
         "?list=red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{?list*}")
         var-types
         "?list=red&list=green&list=blue")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{?keys}")
         var-types
         "?keys=semi,%3B,dot,.,comma,%2C")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{?keys*}")
         var-types
         "?semi=%3B&dot=.&comma=%2C"))))

    (testing "Form-Style Query Continuation: {&var}"

      (is
       (=
        (select-keys variables [:who])
        (match-uri
         (compile-uri-template "{&who}")
         var-types
         "&who=fred")))

      (is
       (=
        (select-keys variables [:half])
        (match-uri
         (compile-uri-template "{&half}")
         var-types
         "&half=50%25")))

      (is
       (=
        (select-keys variables [:x])
        (match-uri
         (compile-uri-template "?fixed=yes{&x}")
         var-types
         "?fixed=yes&x=1024")))

      (is
       (=
        (select-keys variables [:x :y :empty])
        (match-uri
         (compile-uri-template "{&x,y,empty}")
         var-types
         "&x=1024&y=768&empty=")))

      (is
       (=
        (select-keys variables [:x :y])
        (match-uri
         (compile-uri-template "{&x,y,undef}")
         var-types
         "&x=1024&y=768")))

      (is
       (=
        (-> (select-keys variables [:var])
            (update :var subs 0 3))
        (match-uri
         (compile-uri-template "{&var:3}")
         var-types
         "&var=val")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{&list}")
         var-types
         "&list=red,green,blue")))

      (is
       (=
        (select-keys variables [:list])
        (match-uri
         (compile-uri-template "{&list*}")
         var-types
         "&list=red&list=green&list=blue")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{&keys}")
         var-types
         "&keys=semi,%3B,dot,.,comma,%2C")))

      (is
       (=
        (select-keys variables [:keys])
        (match-uri
         (compile-uri-template "{&keys*}")
         var-types
         "&semi=%3B&dot=.&comma=%2C"))))))

(deftest readme-test
  (let [uri-template
        (compile-uri-template "https://{environment}bank.com{/ctx*,accno}/transactions{.format}{?from,to}{#fragment}")]

    (is (= "https://bank.com/accounts/12345678/transactions.csv?from=20201010&to=20201110"
           (make-uri uri-template
                     {:accno "12345678"
                      :ctx "accounts"
                      :format "csv"
                      :from "20201010"
                      :to "20201110"})))

    (is (= "https://test.env1.bank.com/accounts/12345678/transactions.csv?from=20201010&to=20201110"
           (make-uri uri-template
                     {:environment "test.env1."
                      :accno "12345678"
                      :ctx "accounts"
                      :format "csv"
                      :from "20201010"
                      :to "20201110"})))

    (testing "Missing fragment"
      (is (= {:environment ""
              :ctx ["test" "accounts"]
              :accno "12345678"
              :format "csv"
              :from "20201010"
              :to "20201110"
              :fragment nil}
             (match-uri uri-template
                        {:environment :string
                         :fragment :string
                         :accno :string
                         :ctx :list
                         :format :string
                         :from :string
                         :to :string}
                        "https://bank.com/test/accounts/12345678/transactions.csv?from=20201010&to=20201110"))))

    (testing "Missing query string"
      (is (= {:environment ""
              :ctx ["test" "accounts"]
              :accno "12345678"
              :format "csv"
              :from nil
              :to nil
              :fragment nil}
             (match-uri uri-template
                        {:environment :string
                         :fragment :string
                         :accno :string
                         :ctx :list
                         :format :string
                         :from :string
                         :to :string}
                        "https://bank.com/test/accounts/12345678/transactions.csv"))))))
