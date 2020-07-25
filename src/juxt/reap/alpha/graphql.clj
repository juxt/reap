;; Copyright © 2020, JUXT LTD.

(ns juxt.reap.alpha.graphql
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.api :as reap]
   [juxt.reap.alpha.combinators :as p]))

;; Negative lookahead
;;(?!X) 	X, via zero-width negative lookahead

;; 2.1 Source Text

(def SourceCharacter
  #"[\u0009\u000A\u000D\u0020-\uFFFF]")

;; 2.1.1 Unicode

(def UnicodeBOM
  #"\uFEFF")

;; 2.1.2 White Space

(def WhiteSpace
  #"[\u0009\u0020]")

;; 2.1.3 Line Terminators

(def LineTerminator
  #"(?:\u000A|\u000D(?!\u000A)|\u000D\u000A)")

;; 2.1.4 Comments

;; TODO

;; 2.1.5 Insignificant Commas

(def Comma #",")

;; 2.1.6 Lexical Tokens

(declare Punctuator)
(declare Name)
(declare IntValue)
;;(declare FloatValue)
(declare StringValue)

(def Token
  (p/alternatives
   (p/array-map :punctuator Punctuator)
   (p/array-map :name Name)
   (p/array-map :int-value IntValue)
   ;;(p/array-map :float-value FloatValue)
   (p/array-map :string-value StringValue)))

;; 2.1.7 Ignored Tokens

(def Ignored*
  (re-pattern (format "(?:%s|%s|%s)*" WhiteSpace LineTerminator Comma)))

(defn as-token [p]
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser Ignored*))
    p
    (p/ignore (p/pattern-parser Ignored*)))))

;; 2.1.8 Punctuators

(def Punctuator
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser Ignored*))
    (p/pattern-parser
     (re-pattern
      "(\\!|\\$|\\(|\\)|\\.\\.\\.|:|=|@|\\[|\\]|\\{|\\|\\})")
     {:group 1})
    (p/ignore (p/pattern-parser Ignored*))
    )))

(def PunctuatorToken (as-token Punctuator))

;; 2.1.9 Names

(def ^{:ref "2.1.9"} Name
  (p/pattern-parser
   (re-pattern
    "([_A-Za-z][_0-9A-Za-z]*)")
   {:group 1}))

(def NameToken (as-token Name))

;; 2.2 Document

(declare Definition)

(def Document
  (p/sequence-group Definition))

(declare ExecutableDefinition)

(def Definition
  (p/alternatives
   ExecutableDefinition
;;   TypeSystemDefinition
;;   TypeSystemExtension
   ))

(declare OperationDefinition)

(def ExecutableDefinition
  (p/alternatives
   OperationDefinition
   ;; FragmentDefinition
   ))

;; 2.3 Operations

(declare SelectionSet)
(declare OperationType)
(declare VariableDefinitions)
(declare Directives)

(def OperationDefinition
  (p/into
   {}
   (p/alternatives
    (p/sequence-group
     (p/as-entry
      :operation-type
      #'OperationType)
     (p/optionally
      (p/as-entry
       :name
       Name))
     (p/optionally
      (p/as-entry
       :variable-definitions
       VariableDefinitions))
     (p/optionally
      (p/as-entry
       :directives
       Directives))
     (p/as-entry
      :selection-set
      #'SelectionSet))
    (p/array-map
     :selection-set
     #'SelectionSet))))

(def OperationType
  (p/alternatives
   (p/literal-string "query")
   (p/literal-string "mutation")
   (p/literal-string "subscription")))

;; 2.4 Selection Sets

(declare Field)

(def SelectionSet
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
        #'Field)
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


;; 2.5 Fields

(declare Arguments)

(def Field
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     :name
     NameToken)
    (p/optionally
     (p/as-entry
      :arguments
      #'Arguments))
    (p/optionally
     (p/as-entry
      :arguments
      #'Directives))
    (p/optionally
     (p/as-entry
      :selection-set
      #'SelectionSet)))))


;; 2.6 Arguments

(declare Argument)

(def Arguments
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser #"\("))
    (p/zero-or-more #'Argument)
    (p/ignore (p/pattern-parser #"\)")))))

(comment
  (Arguments (re/input "(name:4, foo:10)")))

(declare Value)

(def Argument
  (p/into
   {}
   (p/sequence-group
    (p/as-entry :name Name)
    (p/ignore (p/pattern-parser (re-pattern ":")))
    (p/as-entry :value #'Value)
    ))
  )

(comment
  (Argument (re/input "id:4"))
  (Argument (re/input "id: 4")))


;; 2.9 Input Values

(declare BooleanValue)

(def Value
  (p/alternatives
   #'IntValue
   BooleanValue
   StringValue))

;; TODO: ValueToken?

(comment
  (Value (re/input "true  "))
  (Value (re/input "  123  "))
  (Value (re/input "  4\t"))
  (Value (re/input "  \"graphql is cool!\"  ")))

;; 2.9.1 Int Value

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

(def IntValue
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


;; 2.12 Directives

(def Directive
  (p/sequence-group
   (p/literal-string "@")
   NameToken
   Arguments))

(def Directives
  (p/zero-or-more Directive))

;;;;;;;;;;;;;;;;;;;;;;;;;;






(comment
  (re-matches IntegerPart "2891"))



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





(comment
  (Token (re/input " abc  ")))

(comment
  (Token (re/input "  123")))







;; 2.8 Fragments

(declare FragmentName)

(def FragmentDefinition
  (p/sequence-group
   (p/literal-string "fragment")
   FragmentName
;;   TypeCondition
;;   Directives
   SelectionSet))

(def FragmentName
  (p/comp
   (fn [res] (when-not (= res "on") res))
   Name))







;; Example No 10

(comment
  (reap/decode
   SelectionSet
   "{
  user(id: 4) {
    id
    name
    profilePic(size: 100)
  }
}
"))


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
