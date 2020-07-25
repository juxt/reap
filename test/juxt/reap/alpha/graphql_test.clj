;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.graphql-test
  (:require
   [juxt.reap.alpha.graphql :as g]
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.api :as reap]
   [clojure.test :refer [deftest is are testing]]))

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
")))

;; TODO: Parse a full Document (list of Definitions)
#_(deftest document-test
  (is
   (reap/decode g/Document
                "query:
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
