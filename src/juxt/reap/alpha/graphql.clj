;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.graphql
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.api :as reap]
   [juxt.reap.alpha.combinators :as p]))

;; Negative lookahead
;;(?!X) 	X, via zero-width negative lookahead

(def ^{:ref "2.1"} SourceCharacter
  #"[\u0009\u000A\u000D\u0020-\uFFFF]")

(def ^{:ref "2.1.1"} UnicodeBOM
  #"\uFEFF")

(def ^{:ref "2.1.2"} WhiteSpace
  #"[\u0009\u0020]")

(def ^{:ref "2.1.3"} LineTerminator
  #"(?:\u000A|\u000D(?!\u000A)|\u000D\u000A)")

(comment
  ((p/sequence-group
    (p/pattern-parser #"[a-z]+")
    (p/pattern-parser LineTerminator)
    (p/pattern-parser #"[a-z]+"))))

(def ^{:ref "2.1.5"} Comma
  #",")

(def ^{:ref "2.1.7"} Ignored*
  (re-pattern (format "(?:%s|%s|%s)*" WhiteSpace LineTerminator Comma)))

(def ^{:ref "2.1.8"} Punctuator
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser Ignored*))
    (p/pattern-parser
     (re-pattern
      "(\\!|\\$|\\(|\\)|\\.\\.\\.|:|=|@|\\[|\\]|\\{|\\|\\})")
     {:group 1})
    (p/ignore (p/pattern-parser Ignored*))
    )))

(defn as-token [p]
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser Ignored*))
    p
    (p/ignore (p/pattern-parser Ignored*)))))

(def PunctuatorToken (as-token Punctuator))

(comment
  (Punctuator (re/input "  !  ")))

(def ^{:ref "2.1.9"} Name
  (p/pattern-parser
   (re-pattern
    "([_A-Za-z][_0-9A-Za-z]*)")
   {:group 1}))

(def NameToken (as-token Name))

(comment
  (Name (re/input "GraphQL"))
  (Name (re/input "lastName(a:123)")))

(def NegativeSign
  (re-pattern "-"))

(def Digit
  (re-pattern "[0-9]"))

(def NonZeroDigit
  (re-pattern "[1-9]"))

(def IntegerPart
  (re-pattern
   (format
    "(?:%s0)|(?:(?:%s)?%s%s*)"
    NegativeSign
    NegativeSign
    NonZeroDigit
    Digit)))

(comment
  (re-matches IntegerPart "2891"))

(def ^{:ref "2.9.1"} IntValue
  (p/comp
   #(Integer/parseInt %)
   (p/first
    (p/sequence-group
     (p/ignore (p/pattern-parser Ignored*))
     (p/pattern-parser IntegerPart)
     (p/ignore (p/pattern-parser Ignored*))))))

(comment
  (IntValue (re/input "   2891   ")))

(def IntValueToken (as-token IntValue))

(def ^{:ref "2.9.3"} BooleanValue
  (p/comp
   #(Boolean/valueOf %)
   (p/first
    (p/sequence-group
     (p/ignore (p/pattern-parser Ignored*))
     (p/pattern-parser #"true|false")
     (p/ignore (p/pattern-parser Ignored*))))))

(comment
  (BooleanValue (re/input "   false   ")))

;; TODO: This is an approxmiate definition - see spec for more details on
;; escaping rules.
(def ^{:ref "2.9.4"} StringValue
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser Ignored*))
    (p/pattern-parser #"\"([^\"\\\r\n]+)\"" {:group 1})
    (p/ignore (p/pattern-parser Ignored*)))))

(def StringValueToken (as-token StringValue))

(def Value
  (p/alternatives
   IntValue
   BooleanValue
   StringValue))

;; TODO: ValueToken?

(comment
  (Value (re/input "true  "))
  (Value (re/input "  123  "))
  (Value (re/input "  4\t"))
  (Value (re/input "  \"graphql is cool!\"  ")))

(def Token
  (p/alternatives
   (p/array-map :punctuator Punctuator)
   (p/array-map :name Name)
   (p/array-map :int-value IntValue)
   (p/array-map :string-value StringValue)))

(comment
  (Token (re/input " abc  ")))

(comment
  (Token (re/input "  123")))

(def Argument
  (p/into
   {}
   (p/sequence-group
    (p/as-entry :name Name)
    (p/ignore (p/pattern-parser (re-pattern ":")))
    (p/as-entry :value Value)
    ))
  )

(comment
  (Argument (re/input "id:4"))
  (Argument (re/input "id: 4")))

(def Arguments
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser #"\("))
    (p/zero-or-more Argument)
    (p/ignore (p/pattern-parser #"\)")))))

(comment
  (Arguments (re/input "(name:4, foo:10)")))

(declare SelectionSet)
(declare Field)

(def ^{:ref "2.4"} SelectionSet
  (p/first
   (p/sequence-group
    (p/ignore
     (as-token
      (p/pattern-parser
       (re-pattern
        "\\{"))))

    (p/comp
     vec
     (p/zero-or-more
      (p/alternatives
       (p/as-entry
        :field
        Field)
       ;; TODO: FragmentSpread
       ;; TODO: InlineFragment
       )))

    (p/ignore
     (p/expect
      "Did you forget a closing brace?"
      (as-token
       (p/pattern-parser
        (re-pattern
         "\\}"))))))))

(def ^{:ref "2.5"} Field
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     :name
     NameToken)
    (p/optionally
     (p/as-entry
      :arguments
      Arguments))
    (p/optionally
     (p/as-entry
      :selection-set
      #'SelectionSet)))))

(def OperationDefinition
  (p/into
   {}
   (p/alternatives
    (p/sequence-group
     (p/as-entry
      :operation-type
      (p/alternatives
       (p/pattern-parser
        (re-pattern "query"))
       (p/pattern-parser
        (re-pattern "mutation"))
       (p/pattern-parser
        (re-pattern "subscription"))))
     (p/optionally
      (p/as-entry
       :name
       Name))
     #_(p/optionally
        (p/as-entry
         :variable-definitions
         nil))
     #_(p/optionally
        (p/as-entry
         :directives
         nil))
     (p/as-entry
      :selection-set
      SelectionSet))
    (p/array-map
     :selection-set
     SelectionSet))))

(comment
  (reap/decode
   OperationDefinition
   "{
  likeStory(storyID: 12345) {
    story {
      likeCount
    }
  }
}
"))

;; Example No 10

(reap/decode
 SelectionSet
 "{
  user(id: 4) {
    id
    name
    profilePic(size: 100)
  }
}
")


;;"{ id firstName lastName { abc } }"

(comment
  (reap/decode
   SelectionSet
   "{
  me {
    id
    firstName(name: \"Malcolm\")
    lastName
    birthday {
      month
      day
    }
    friends {
      name
    }
  }
}
"))

#_(comment
  (selection-set
   (re/input
    "{
  user(id: 4) {
    name
  }
}")))
