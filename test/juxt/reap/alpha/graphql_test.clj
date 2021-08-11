;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.graphql-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is are]]
   [juxt.reap.alpha.api :as reap]
   [juxt.reap.alpha.graphql :as g]
   [juxt.reap.alpha.graphql.util :as gutil]
   [juxt.reap.alpha.regex :as re]))

(deftest punctuator-test
  (is
   (= "!" (g/Punctuator (re/input "  !  ")))))

(deftest names-test
  (is
   (= "GraphQL" (reap/decode g/Name "GraphQL"))
   (= "lastName" (reap/decode g/Name "lastName(a:123)"))))

(deftest operation-definition-test
  (is
   (reap/decode
    g/OperationDefinition
    "{
  likeStory(storyID: 12345) {
    story {
      likeCount
    }
  }
}
"))
  (is
   (reap/decode
    g/OperationDefinition
    "  query    IntrospectionQuery
{  story     }     ")))

(deftest int-value-test
  (is (= 2891 (reap/decode g/IntValue "   2891   "))))

(deftest document-test
  (is
   (reap/decode g/Document
                "
    query IntrospectionQuery {
      __schema {

        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          ...FullType
        }
        directives {
          name
          description

          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }
  "))

  )

(deftest document2-test
  (is
   (reap/decode g/Document
                "
    query IntrospectionQuery {
      __schema {

        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          ...FullType
        }
        directives {
          name
          description

          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }
  ")))

(comment
  (let [res
        (reap/decode g/Document
                     "
    query IntrospectionQuery {
      __schema {

        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          ...FullType
        }
        directives {
          name
          description

          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }
  ")]
    res
    ))



;; 2.1.4 Comments
#_(deftest comments-test
  (is
   (=
    [{::g/selection-set
      [[::g/field {::g/name "foo", ::g/arguments {}}]
       [::g/field {::g/name "bar", ::g/arguments {"a" 10}}]]}]
    (reap/decode g/Document "{ foo # a comment \r\nbar(a: # comment \n10)  } # another comment"))))

;; 2.8 Fragments

;; Example No. 20

#_(deftest fragments-test
  (is
   (= {::g/operation-type "query",
       ::g/name "withNestedFragments",
       ::g/selection-set
       [[::g/field
         {::g/name "user",
          ::g/arguments {"id" 4},
          ::g/selection-set
          [[::g/field
            {::g/name "friends",
             ::g/arguments {"first" 10},
             ::g/selection-set
             [[::g/field {::g/name "id", ::g/arguments {}}]
              [::g/field {::g/name "name", ::g/arguments {}}]
              [::g/field {::g/name "profilePic", ::g/arguments {"size" 50}}]]}]
           [::g/field
            {::g/name "mutualFriends",
             ::g/arguments {"first" 10},
             ::g/selection-set
             [[::g/field {::g/name "id", ::g/arguments {}}]
              [::g/field {::g/name "name", ::g/arguments {}}]
              [::g/field {::g/name "profilePic", ::g/arguments {"size" 50}}]]}]]}]]}
      (let [doc (reap/decode
                 g/Document
                 (slurp (io/resource "juxt/reap/alpha/graphql/example20.graphql")))]
        (gutil/deref-fragments (first doc) doc)))))

(comment
  (let [doc (reap/decode
             g/Document
             (slurp (io/resource "juxt/reap/alpha/graphql/example20.graphql")))]
    (gutil/deref-fragments (first doc) doc)))


;; 2.9.4 String Value

;; (reap/decode g/StringValue "\"\"\"hello\"\"\"")

;;


#_(reap/decode
         g/Document
         "type Query { myName: [String]!}")

#_(reap/decode
         g/ListType
         "[String]")

#_(reap/decode
         g/NonNullType
         "String!")

#_(reap/decode
         g/NamedType
         "String")

(deftest parse-wrapping-types-test
  (are [expr path expected]
      (= expected
         (get-in (first (reap/decode
                         g/Document
                         (format "type Query { amt: %s }" expr)))
                 (into [::g/fields 0 ::g/type] path)))
    "Int" [] "Int"
    "[Int]" [::g/type] :list
    "[Int]" [::g/item-type] "Int"
    "Int!" [::g/type] :non-null
    "Int!" [::g/inner-type] "Int";
    "[Int]!" [::g/type] :non-null
    "[Int]!" [::g/inner-type ::g/type] :list
    "[Int]!" [::g/inner-type ::g/item-type] "Int"))

(reap/decode
 g/Document
 (format "type Query { amt: [[[[Int!]]]]! }"))

#_(deftest parse-non-null-type-test
  (let [actual
        (reap/decode
         g/Document
         "type Query { myName: String! }")]
    (is (= "String" (get-in (first actual) [::g/fields 0 ::g/type ::g/inner-type])))))


#_(deftest inline-fragment-parsing-test
  (is
   (reap/decode
    g/Document
    "query inlineFragmentTyping { profiles(handles: [10, 20]) }")))

#_(reap/decode
    g/Document
    "query inlineFragmentTyping {
profiles(handles: [10, 20])
}
")
