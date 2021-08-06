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

(def Comment #"\#[\u0009\u0020-\uFFFF]*")

;; 2.1.5 Insignificant Commas

(def Comma #",")

;; 2.1.6 Lexical Tokens

(declare Punctuator Name IntValue #_FloatValue StringValue)

(def Token
  (p/alternatives
   (p/array-map :punctuator Punctuator)
   (p/array-map :name Name)
   (p/array-map :int-value #'IntValue)
   ;;(p/array-map :float-value FloatValue)
   (p/array-map :string-value #'StringValue)))

;; 2.1.7 Ignored Tokens

(def Ignored*
  (re-pattern (format "(?:%s|%s|%s|%s)*" WhiteSpace LineTerminator Comment Comma)))

(defn as-token [p]
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser Ignored*))
    p
    (p/ignore (p/pattern-parser Ignored*)))))

(defn token [s]
  (as-token (p/string-literal s)))

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

(declare OperationDefinition FragmentDefinition)

(def ExecutableDefinition
  (p/alternatives
   #'OperationDefinition
   #'FragmentDefinition
   ))

(declare TypeSystemDefinition)

(def Definition
  (p/alternatives
   ExecutableDefinition
   #'TypeSystemDefinition
;;   TypeSystemExtension
   ))

(def Document
  (p/complete
   (p/one-or-more Definition)))

;; 2.3 Operations

(declare SelectionSet OperationType VariableDefinitions Directives)

(def OperationDefinition
  (p/into
   {}
   (p/alternatives
    (p/sequence-group
     (p/as-entry
      ::operation-type
      #'OperationType)
     (p/optionally
      (p/as-entry
       ::name
       NameToken))
     (p/optionally
      (p/as-entry
       ::variable-definitions
       #'VariableDefinitions))
     (p/optionally
      (p/as-entry
       ::directives
       #'Directives))
     (p/as-entry
      ::selection-set
      #'SelectionSet))
    (p/array-map
     ::selection-set
     #'SelectionSet))))

(def OperationType
  (p/alternatives
   (token "query")
   (token "mutation")
   (token "subscription")))

;; 2.4 Selection Sets

(declare Field FragmentSpread)

(def SelectionSet
  (p/first
   (p/sequence-group
    (p/ignore
     (token "{"))
    (p/one-or-more
     (p/alternatives
      (p/array-map
       ::field
       #'Field)
      (p/array-map
       ::fragment-spread
       #'FragmentSpread)
      ;; TODO: InlineFragment
      ))

    (p/ignore
     (token "}")

     #_(p/expect
        "Did you forget a closing brace?"
        (token "}"))))))

(comment
  (SelectionSet (re/input "{foo(a:10) bar ...zip}")))

;; 2.5 Fields

(declare Alias Arguments)

(def Field
  (p/into
   {}
   (p/sequence-group
    ;; Need some lookahead to distinguish between Alias and Name
    #_(p/optionally
     (p/as-entry
      :alias
      #'Alias))
    (p/as-entry
     ::name
     NameToken)
    (p/optionally
     (p/as-entry
      ::arguments
      #'Arguments))
    (p/optionally
     (p/as-entry
      ::directives
      #'Directives))
    (p/optionally
     (p/as-entry
      ::selection-set
      #'SelectionSet)))))


;; 2.6 Arguments

(declare Argument)

(def Arguments
  (p/into
   {}
   (p/map
    (juxt ::name ::value)
    (p/first
     (p/sequence-group
      (p/ignore (token "("))
      (p/one-or-more #'Argument)
      (p/ignore (token ")")))))))

(comment
  (Arguments (re/input "(name:4, foo:10)")))

(declare Value)

(def Argument
  (p/into
   {}
   (p/sequence-group
    (p/as-entry ::name Name)
    (p/ignore (token ":"))
    (p/as-entry ::value #'Value))))

(comment
  (Argument (re/input "id:4"))
  (Argument (re/input "id: 4")))

;; 2.7 Field Alias

(def Alias
  (p/sequence-group
   NameToken (token ":")))

;; 2.8 Fragments

(def FragmentName
  (p/comp
   (fn [res] (when-not (= res "on") res))
   Name))

(declare TypeCondition)

(def FragmentDefinition
  (p/into
   {}
   (p/sequence-group
    (p/ignore
     (token "fragment"))
    (p/as-entry
     ::fragment-name
     FragmentName)
    #'TypeCondition
    (p/as-entry
     ::directives
     (p/optionally
      #'Directives))
    (p/as-entry
     ::selection-set
     SelectionSet))))

(def FragmentSpread
  (p/into
   {}
   (p/sequence-group
    (p/ignore
     (token "..."))
    (p/as-entry
     :fragment-name
     FragmentName)
    (p/optionally
     (p/as-entry
      :directives
      #'Directives)))))

;; 2.8.1 Type Conditions

(declare NamedType)

(def TypeCondition
  (p/first
   (p/sequence-group
    (p/ignore
     (token "on"))
    (p/as-entry
     ::named-type
     #'NamedType))))

;; 2.9 Input Values

(declare BooleanValue)

(def Value
  (p/alternatives
   #'IntValue
   #'BooleanValue
   #'StringValue))

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

;; 2.9.6 Enum Value

(def EnumValue
  Name) ;; TODO: but not 'true', 'false' or 'null'

;; 2.10 Variables

(declare Type)

(def DefaultValue
  (p/sequence-group
   (token "=")
   Value))

(def Variable
  (p/sequence-group
   (token "$")
   NameToken))

(def VariableDefinition
  (p/sequence-group
   Variable
   (token ":")
   #'Type
   (p/optionally
    DefaultValue)))

(def VariableDefinitions
  (p/sequence-group
   (token "(")
   (p/pattern-parser #"\(")
   (p/one-or-more VariableDefinition)
   (token ")")))

;; 2.11 Type References

(declare NamedType NamedTypeNoBang ListType ListTypeNoBang NonNullType)

(def Type
  (p/alternatives
   #'NamedTypeNoBang
   #'ListTypeNoBang
   #'NonNullType))

(def NamedType Name)

(def NamedTypeNoBang
  (p/pattern-parser
   (re-pattern
    (str "([_A-Za-z][_0-9A-Za-z]*)"
         ;; We add some negative lookahead to reject if followed by a bang (!)
         "(?!\\!)"
         ;; but with some positive lookahead to avoid a relunctant match.
         "(?=[^_0-9A-Za-z]|\\z)"))
   {:group 1}))

(def ListType
  (p/into
   {::type :list}
   (p/sequence-group
    (p/ignore (token "["))
    (p/as-entry ::item-type Type)
    (p/ignore (token "]")))))

(def ListTypeNoBang
  (p/into
   {::type :list}
   (p/sequence-group
    (p/ignore (token "["))
    (p/as-entry ::item-type Type)
    (p/ignore (p/pattern-parser (re-pattern (str "\\]"
                                                 "(?!\\!)")))))))
(def NonNullType
  (p/into
   {::type :non-null}
   (p/sequence-group
    (p/alternatives
     (p/as-entry ::nullable-type (p/first (p/sequence-group NamedType (p/ignore (token "!")))))
     (p/as-entry ::nullable-type (p/first (p/sequence-group ListType (p/ignore (token "!")))))))))

;; 2.12 Directives

(def Directive
  (p/sequence-group
   (p/ignore
    (token "@"))
   NameToken
   Arguments))

(def Directives
  (p/into
   {}
   (p/comp
    (comp seq vec)
    (p/one-or-more Directive))))


;;;;;;;;;;;;;;;;;;;;;;;;;;

(def BooleanValue
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
(def StringCharacter
  #"[\u0009\u0020\u0021\u0023-\u005B\u005D-\uFFFF]|\\u[0-9A-Fa-f]{4}|\\[\"\\\/bfnrt]")

(def StringValue
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser Ignored*))
    (p/pattern-parser
     (re-pattern
      (format "\\\"((?:%s)*)\\\"" StringCharacter))
     {:group 1})
    (p/ignore (p/pattern-parser Ignored*)))))

;; TODO: BlockStringCharacter

(def StringValueToken
  (as-token StringValue))

(comment
  (Token (re/input " abc  ")))

(comment
  (Token (re/input "  123")))


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

;; 3. Type System

(declare SchemaDefinition TypeDefinition RootOperationTypeDefinition)

(def TypeSystemDefinition
  (p/alternatives
   #'SchemaDefinition
   #'TypeDefinition
   ;;DirectiveDefinition
   )
  )

;; 3.2 Descriptions

(def Description StringValue)

;; 3.3 Schema

(def SchemaDefinition
  (p/into
   {::type "SchemaDefinition"}
   (p/sequence-group
    (p/ignore (token "schema"))
    (p/optionally
     (p/as-entry
      ::directives
      #'Directives))
    (p/ignore (token "{"))
    (p/as-entry
     ::root-operation-types
     (p/vec (p/one-or-more #'RootOperationTypeDefinition)))
    (p/ignore (token "}")))))

(def RootOperationTypeDefinition
  (p/into {}
          (p/sequence-group
           (p/as-entry ::operation-type #'OperationType)
           (p/ignore (token ":"))
           (p/as-entry ::named-type #'NamedType))))

(comment
  (reap/decode SchemaDefinition "schema {  }")
  (reap/decode SchemaDefinition "schema { query: MyQueryRootType }")
  (reap/decode SchemaDefinition "schema { query: MyQueryRootType  mutation: MyMutationRootType }"))

;; 3.4 Types

(declare ScalarTypeDefinition ObjectTypeDefinition InterfaceTypeDefinition
         UnionTypeDefinition EnumTypeDefinition InputObjectTypeDefinition)

(def TypeDefinition
  (p/alternatives
   #'ScalarTypeDefinition
   #'ObjectTypeDefinition
   #'InterfaceTypeDefinition
   #'UnionTypeDefinition
   #'EnumTypeDefinition
   #'InputObjectTypeDefinition))

;; 3.5 Scalars

(def ScalarTypeDefinition
  (p/into
   {::type "ScalarTypeDefinition"}
   (p/sequence-group
    ;;(p/optionally #'Description)
    (p/ignore (token "scalar"))
    (p/as-entry ::name Name)
    (p/as-entry ::directives (p/optionally Directives)))))


(comment ; example  42
  (reap/decode
   ScalarTypeDefinition
   "scalar Time}"))

(declare ImplementsInterfaces FieldsDefinition)

;; 3.6 Objects

(def ObjectTypeDefinition
  (p/into
   {::type "ObjectTypeDefinition"}
   (p/sequence-group
    ;;(p/optionally #'Description)
    (p/ignore (token "type"))
    (p/as-entry ::name #'Name)
    (p/optionally (p/as-entry ::interfaces #'ImplementsInterfaces))
    (p/optionally (p/as-entry ::directives Directives))
    (p/as-entry ::fields (p/optionally #'FieldsDefinition)))))

(def ImplementsInterfaces
  (p/alternatives
   (p/first
    (p/sequence-group
     (p/ignore (token "implements"))
     (p/optionally (p/ignore (token "&")))
     (p/vec
      (p/cons
       NamedType
       (p/zero-or-more
        (p/first
         (p/sequence-group
          (p/ignore (token "&"))
          NamedType)))))))))

(declare FieldDefinition)

(def FieldsDefinition
  (p/first
   (p/sequence-group
    (p/ignore (token "{"))
    (p/vec
     (p/one-or-more
      #'FieldDefinition))
    (p/ignore (token "}")))))

(declare ArgumentsDefinition)

(def FieldDefinition
  (p/into
   {}
   (p/sequence-group
    (p/optionally #'Description)
    (p/as-entry ::name Name)
    (p/as-entry ::args (p/optionally #'ArgumentsDefinition))
    (p/ignore (token ":"))
    (p/as-entry ::type Type)
    (p/as-entry ::directives (p/optionally Directives)))))

;; 3.6.1 Field Arguments

(declare InputValueDefinition)

(def ArgumentsDefinition
  (p/first
   (p/sequence-group
    (p/ignore (token "("))
    (p/vec (p/one-or-more #'InputValueDefinition))
    (p/ignore (token ")")))))

(def InputValueDefinition
  (p/into {}
   (p/sequence-group
    (p/optionally Description)
    (p/as-entry ::name Name)
    (p/ignore (token ":"))
    (p/as-entry ::type Type)
    (p/optionally DefaultValue)
    (p/optionally Directives))))

(comment
  (reap/decode
   TypeDefinition
   "type Person {
  name: String
  picture(size: Int): Url}"))

;; 3.7 Interfaces

(def InterfaceTypeDefinition
  (p/sequence-group
   (p/optionally Description)
   (p/ignore (token "interface"))
   (p/as-entry ::name Name)
   (p/optionally #'ImplementsInterfaces)
   (p/optionally Directives)
   (p/optionally FieldsDefinition)))

(comment ; example no. 64
  (reap/decode
   InterfaceTypeDefinition
   "interface NamedEntity {
  name: String
}"))

(comment
  (reap/decode
   TypeDefinition
   "type Business implements NamedEntity & ValuedEntity {
  name: String
  value: Int
  employeeCount: Int
}"))

;; 3.8 Unions

(declare UnionMemberTypes)

(def UnionTypeDefinition
  (p/into
   {::type "UnionTypeDefinition"}
   (p/sequence-group
    (p/optionally Description)
    (p/ignore (token "union"))
    (p/as-entry ::name Name)
    (p/optionally (p/as-entry ::directives Directives))
    (p/optionally (p/as-entry ::union-member-types #'UnionMemberTypes)))))

(def UnionMemberTypes
  (p/first
   (p/sequence-group
    (p/ignore (token "="))
    (p/optionally (p/ignore (token "|")))
    (p/vec
     (p/cons
      NamedType
      (p/zero-or-more
       (p/first
        (p/sequence-group
         (p/ignore (token "|"))
         NamedType))))))))

(comment
  (reap/decode
   UnionTypeDefinition
   "union SearchResult = Photo | Person"))

;; 3.9 Enums

(declare EnumValuesDefinition EnumValueDefinition EnumValue)

(def EnumTypeDefinition
  (p/into
   {::type "EnumTypeDefinition"}
   (p/sequence-group
    (p/optionally Description)
    (p/ignore (token "enum"))
    (p/as-entry :name Name)
    (p/optionally (p/as-entry ::directives Directives))
    (p/as-entry :values #'EnumValuesDefinition))))

(def EnumValuesDefinition
  (p/first
   (p/sequence-group
    (p/ignore (token "{"))
    (p/vec (p/one-or-more #'EnumValueDefinition))
    (p/ignore (token "}")))))

(def EnumValueDefinition
  (p/first
   (p/sequence-group
    (p/optionally Description)
    #'EnumValue
    (p/optionally (p/as-entry ::directives Directives)))))

(comment
  (reap/decode
   EnumTypeDefinition
   "enum Direction {
  NORTH
  EAST
  SOUTH
  WEST
}
"))


;; 3.10 Input Objects

(declare InputFieldsDefinition)

(def InputObjectTypeDefinition
  (p/into
   {::type "InputObjectTypeDefinition"}
   (p/sequence-group
    (p/optionally Description)
    (p/ignore (token "input"))
    (p/as-entry ::name Name)
    (p/optionally (p/as-entry ::directives Directives))
    (p/optionally (p/as-entry ::input-fields #'InputFieldsDefinition)))))

(def InputFieldsDefinition
  (p/first
   (p/sequence-group
    (p/ignore (token "{"))
    (p/vec (p/one-or-more InputValueDefinition))
    (p/ignore (token "}")))))

(comment
  (reap/decode
   InputObjectTypeDefinition
   "input Point2D {
  x: Float
  y: Float}
"))
