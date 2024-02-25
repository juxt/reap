;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.adoc-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.regex :as re]
   [juxt.reap.combinators :as p]
   [clojure.string :as str]
   [clojure.java.io :as io]))

;; from https://github.com/asciidocj/asciidocj
;; Document      ::= (Header?,Preamble?,Section*)

;; Header        ::= (Title,(AuthorInfo,RevisionInfo?)?)
;; AuthorInfo    ::= (FirstName,(MiddleName?,LastName)?,EmailAddress?)
;; RevisionInfo  ::= (RevisionNumber?,RevisionDate,RevisionRemark?)
;; Preamble      ::= (SectionBody)
;; Section       ::= (Title,SectionBody?,(Section)*)
;; SectionBody   ::= ((BlockTitle?,Block)|BlockMacro)+
;; Block         ::= (Paragraph|DelimitedBlock|List|Table)
;; List          ::= (BulletedList|NumberedList|LabeledList|CalloutList)
;; BulletedList  ::= (ListItem)+
;; NumberedList  ::= (ListItem)+
;; CalloutList   ::= (ListItem)+
;; LabeledList   ::= (ListEntry)+
;; ListEntry     ::= (ListLabel,ListItem)
;; ListLabel     ::= (ListTerm+)
;; ListItem      ::= (ItemText,(List|ListParagraph|ListContinuation)*)

;; "The line is a significant construct in AsciiDoc. A line is defined
;; as text that’s separated on either side by either a newline
;; character or the boundary of the document." --
;; https://docs.asciidoctor.org/asciidoc/latest/document-structure/

;; Therefore, we define a line as this regex pattern: ^.*(?:\n|\z)

(defn normalize
  "Normalize all the lines of the given string, as per
  https://docs.asciidoctor.org/asciidoc/latest/normalization/"
  [s]
  (->>
   (str/split s #"\h*\R") ; trim trailing whitespace
   ))

(def adoc-comment-line
  (p/pattern-parser #"^(//.*)(?:\n|\z)" {:group 1}))

(def doctitle
  (p/comp
   (fn [{:keys [doctitle]}]
     (let [segments
           ;; TODO: See
           ;; https://docs.asciidoctor.org/asciidoc/latest/document/subtitle/ for
           ;; discussion about assigning a custom separator to the document
           ;; title. Not yet implemented.
           (str/split doctitle #":\h+")]
       (if (< (count segments) 2)
         {:title (first segments)}
         {:title (str/join ": " (butlast segments))
          :subtitle (last segments)})))
   (p/pattern-parser #"=\h+(.*)(?:\n|\z)" {:group {:doctitle 1}})))

(defn decode-compound-name
  "Decode a compound name.

  Compound author names are names that contain spaces, such as 'Ann
  Marie'. To support these, we encode a space with an underscore
  https://docs.asciidoctor.org/asciidoc/latest/document/compound-author-name/."

  [s] (str/replace s \_ \space))

(def firstname
  (p/comp
   decode-compound-name
   (p/pattern-parser #"[\p{IsAlphabetic}\p{Graph}&&[^;<:]]+")))

(comment
  (firstname (input "Ann_Marie")))

(def middlename
  (p/comp
   decode-compound-name
   (p/pattern-parser #"[\p{IsAlphabetic}\p{Graph}&&[^;<:]]+")))

(def lastname
  (p/comp
   decode-compound-name
   (p/pattern-parser #"[\p{IsAlphabetic}\p{Graph}&&[^;<:]]+")))

(comment
  (lastname (input "López_del_Toro")))

;; See https://stackoverflow.com/questions/8204680/java-regex-email
(def email (p/pattern-parser #"(?i)^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,6}"))

(defn assoc-author-initials [author]
  (assoc
   author :authorinitials
   (str/join
    (keep
     #(when % (subs % 0 1))
     ((juxt :firstname :middlename :lastname) author)))))

(def author
  (p/comp
   assoc-author-initials
   (p/as-map
    (p/alternatives
     ;; Firstname Middlename Lastname
     (p/sequence-group
      (p/as-entry
       :firstname
       firstname)
      (p/as-entry
       :middlename
       (p/first
        (p/sequence-group
         (p/ignore (p/pattern-parser #"\h+"))
         middlename)))
      (p/as-entry
       :lastname
       (p/first
        (p/sequence-group
         (p/ignore (p/pattern-parser #"\h+"))
         lastname))))
     ;; Firstname Lastname
     (p/sequence-group
      (p/as-entry
       :firstname
       firstname)
      (p/as-entry
       :lastname
       (p/first
        (p/sequence-group
         (p/ignore (p/pattern-parser #"\h+"))
         lastname))))
     ;; Firstname
     (p/sequence-group
      (p/as-entry
       :firstname
       firstname))))))

(def email-with-angle-brackets
  (p/first
   (p/sequence-group
    (p/ignore (p/pattern-parser #"\<"))
    email
    (p/ignore (p/pattern-parser #"\>")))))

(def author-info
  (p/as-map
   (p/sequence-group
    (p/as-entry
     :author
     author)
    (p/optionally
     (p/as-entry
      :email
      (p/first
       (p/sequence-group
        (p/ignore (p/pattern-parser #"\h+"))
        email-with-angle-brackets)))))))

(def author-line
  (p/first
   (p/sequence-group
    (p/comp
     vec
     (p/cons
      author-info
      (p/first
       (p/sequence-group
        (p/zero-or-more
         (p/first
          (p/sequence-group
           (p/ignore (p/pattern-parser #";\h+"))
           author-info)))))))
    (p/ignore (p/pattern-parser #"(?:\n|\z)")))))

(defn trim-vals [m]
  (update-vals m str/trim))

(def revision-line
  (p/comp
   trim-vals
   (p/alternatives
    (p/pattern-parser
     #"^v?[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)\h*\,\h*(?<RevisionDate>[\p{Alnum}-,\h]+)\h*\:\h*(?<Remark>.*)(?:\n|\z)"
     {:group
      {:revision-number "RevisionNumber"
       :revision-date "RevisionDate"
       :revision-remark "Remark"}})

    ;; "When the revision line contains a version and a date, separate
    ;; the version number from the date with a comma"
    (p/pattern-parser
     #"v?[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)\h*(?:,\h*(?<RevisionDate>[\p{Alnum}\-,\h]+))(?:\n|\z)"
     {:group
      {:revision-number "RevisionNumber"
       :revision-date "RevisionDate"}})

    (p/pattern-parser
     #"v?[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)\h*(?:\:\h*(?<Remark>.*))?(?:\n|\z)"
     {:group
      {:revision-number "RevisionNumber"
       :revision-remark "Remark"}})

    ;; "When the revision line only contains a revision number, prefix
    ;; the number with a v." --
    ;; https://docs.asciidoctor.org/asciidoc/latest/document/revision-line/
    (p/pattern-parser
     #"v[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)(?:\n|\z)"
     {:group {:revision-number "RevisionNumber"}}))))

;; https://docs.asciidoctor.org/asciidoc/latest/attributes/attribute-entries/
(def attribute-entry
  (p/alternatives
   ;; Normal attribute
   (p/comp
    (fn [{:keys [attribute-name attribute-value]}]
      (when-let [[_ attribute-name] (re-matches #"([^!]+)\!?" attribute-name)]
        {:attribute-name attribute-name
         :attribute-value (str/trim attribute-value)}))
    (p/pattern-parser
     #"^\:([^:]+)\:\h+(\S.*)(?:\n|\z)"
     {:group
      {:attribute-name 1
       :attribute-value 2}}))
   ;; Boolean attribute
   (p/comp
    (fn [{:keys [attribute-label]}]
      (let [[_ attribute-name] (re-matches #"([^!]+)\!?" attribute-label)]
        {:attribute-name attribute-name
         :attribute-value (when (not (str/ends-with? attribute-label "!")) "")}))
    (p/pattern-parser
     #"^\:([^:]+)\:(?:\n|\z)"
     {:group
      {:attribute-label 1}}))))

(comment
  (attribute-entry (input ":name-of-an-attribute:   value of the attribute  \n")))

(def block-element-delimiter
  (p/pattern-parser
   #"^(\/{4,}|\={4,}|\-{4,}|\.{4,}|\-{2}|\*{4,}|\|={3}|\,={3}|\:={3}|\!={3}|\+{4,}|\_{4,})(?:\n|\z)"
   {:group 1}))

(comment
  (block-element-delimiter (re/input "====")))

(deftest parser-test
  (testing "document title"
    (is (doctitle (re/input "= The Intrepid Chronicles\nfoo")))
    (is (not (doctitle (re/input "The Intrepid Chronicles"))))
    (is
     (= {:title "foo"}
        (doctitle (re/input "= foo"))))
    (is
     (= {:title "foo"
         :subtitle "bar"}
        (doctitle (re/input "= foo: bar"))))
    (is
     (= {:title "foo: bar" :subtitle "zip"}
        (doctitle (re/input "= foo: bar: zip")))))

  (testing "author"
    (is (author (re/input "Malcolm")))
    (is (author (re/input "Malcolm Sparks")))
    (is (author (re/input "Malcolm James Sparks"))))

  (testing "compound names in the author line"
    (is
     (=
      [{:author {:firstname "Ann Marie" :lastname "Jenson" :authorinitials "AJ"}}
       {:author {:firstname "Tomás" :lastname "López del Toro" :authorinitials "TL"}}]
      (author-line (re/input "Ann_Marie Jenson; Tomás López_del_Toro")))))

  (testing "email"
    (is (email (re/input "mal@juxt.pro")))
    (is (email (re/input "MAL@JUXT.PRO")))
    (is (not (email (re/input "mal")))))

  (testing "multiple authors"
    (is
     (=
      [{:author
        {:firstname "Kismet"
         :middlename "R."
         :lastname "Lee"
         :authorinitials "KRL"}
        :email "kismet@asciidoctor.org"}
       {:author {:firstname "B."
                 :lastname "Steppenwolf"
                 :authorinitials "BS"}}
       {:author {:firstname "Pax"
                 :lastname "Draeke"
                 :authorinitials "PD"}
        :email "pax@asciidoctor.org"}]
      (author-line
       (re/input
        "Kismet R. Lee <kismet@asciidoctor.org>; B. Steppenwolf; Pax Draeke <pax@asciidoctor.org>")))))

  (testing "revision line"
    (is (= {:revision-number "7.5"}
           (revision-line (re/input "v7.5"))))
    (is (= {:revision-number "7.5" :revision-date "1-29-2020"}
           (revision-line (re/input "7.5, 1-29-2020"))))
    (is (= {:revision-number "7.5" :revision-date "1-29-2020"}
           (revision-line (re/input "v7.5, 1-29-2020"))))
    (is (= {:revision-number "7.5"
            :revision-remark "A new analysis"}
           (revision-line (re/input "7.5: A new analysis"))))
    (is (= {:revision-number "7.5"
            :revision-remark "A new analysis"}
           (revision-line (re/input "v7.5: A new analysis"))))
    (is (= {:revision-number "7.5"
            :revision-date "1-29-2020"
            :revision-remark "A new analysis"}
           (revision-line (re/input "7.5, 1-29-2020: A new analysis"))))
    (is (= {:revision-number "7.5"
            :revision-date "1-29-2020"
            :revision-remark "A new analysis"}
           (revision-line (re/input "v7.5, 1-29-2020: A new analysis")))))

  ;; TODO: Restore this
  #_(testing "attributes"
    (is (= {:attribute-name "name-of-an-attribute"
            :attribute-value ""}
           (attribute-entry (re/input ":name-of-an-attribute:  "))))
    (is (= {:attribute-name "name-of-an-attribute"
            :attribute-value "value of the attribute"}
           (attribute-entry (re/input ":name-of-an-attribute: value of the attribute  "))))
    (is (= {:attribute-name "name-of-an-attribute"
            :attribute-value nil}
           (attribute-entry (re/input ":name-of-an-attribute!: "))))
    (is (not (attribute-entry (re/input ":foo:bar")))))

  (testing "block element delimiter"
    (is (block-element-delimiter (re/input "////")))
    (is (block-element-delimiter (re/input "//////")))
    (is (not (block-element-delimiter (re/input "///"))))

    (is (block-element-delimiter (re/input "====")))
    (is (block-element-delimiter (re/input "======")))
    (is (not (block-element-delimiter (re/input "==="))))

    (is (block-element-delimiter (re/input "----")))
    (is (block-element-delimiter (re/input "------")))
    (is (not (block-element-delimiter (re/input "---"))))

    (is (block-element-delimiter (re/input "....")))
    (is (block-element-delimiter (re/input "......")))
    (is (not (block-element-delimiter (re/input "..."))))

    (is (block-element-delimiter (re/input "--")))
    (is (not (block-element-delimiter (re/input "-"))))
    (is (not (block-element-delimiter (re/input "---"))))

    (is (block-element-delimiter (re/input "|===")))
    (is (not (block-element-delimiter (re/input "|=="))))
    (is (not (block-element-delimiter (re/input "|===="))))
    (is (block-element-delimiter (re/input ",===")))
    (is (block-element-delimiter (re/input ":===")))
    (is (block-element-delimiter (re/input "!===")))

    (is (block-element-delimiter (re/input "++++")))
    (is (block-element-delimiter (re/input "++++++")))
    (is (not (block-element-delimiter (re/input "+++"))))))

;; TODO: Escape a trailing character reference (https://docs.asciidoctor.org/asciidoc/latest/document/multiple-authors/)
;; TODO: Assign Author and Email with Attribute Entries (https://docs.asciidoctor.org/asciidoc/latest/document/author-attribute-entries/)
;; TODO: Reference the Author Information (https://docs.asciidoctor.org/asciidoc/latest/document/reference-author-attributes/)

(defn parse-document-lines [lines]
  (let [state-transition-model
        {:start
         (fn [acc line]
           (if-let [comment-line (adoc-comment-line (re/input line))]
             (-> acc
                 (update :head-comment-lines (fnil conj []) comment-line))
             (if-let [doctitle (doctitle (re/input line))]
               (-> acc
                   (assoc :doctitle doctitle)
                   (assoc :state :post-doctitle))
               (throw
                (ex-info
                 "Unexpected input"
                 {:state (:state acc)
                  :line line})))))

         :post-doctitle
         (fn [acc line]
           (if-let [author-line (author-line (re/input line))]
             (-> acc
                 (assoc :author-line author-line)
                 (assoc :state :post-author-line))
             (if-let [revision-line (revision-line (re/input line))]
               (-> acc
                   (assoc :revision-line revision-line)
                   (assoc :state :post-revision-line))
               (throw
                (ex-info
                 "Unexpected input"
                 {:state (:state acc)
                  :line line})))))

         :post-author-line
         (fn [acc line]
           (if-let [revision-line (revision-line (re/input line))]
             (-> acc
                 (assoc :revision-line revision-line)
                 (assoc :state :document-attributes))
             (if-let [attribute-entry (attribute-entry (re/input line))]
               (-> acc
                   (update :document-attributes (fnil conj []) attribute-entry)
                   (assoc :state :document-attributes))
               (throw
                (ex-info
                 "Unexpected input"
                 {:state (:state acc)
                  :line line})))))

         :document-attributes
         (fn [acc line]
           (if-let [attribute-entry (attribute-entry (re/input line))]
             (-> acc
                 (update :document-attributes (fnil conj []) attribute-entry))
             (if [(str/blank? line)]
               (assoc acc :state :body)
               (throw
                (ex-info
                 "Unexpected input"
                 {:state (:state acc)
                  :line line})))))

         :body (fn [acc line]
                 (->
                  acc
                  (update :current-block (fnil conj []) line)))}]

    (reduce
     (fn [{:keys [state] :as acc} line]
       (if-let [f (state-transition-model state)]
         (f acc line)
         (throw
          (ex-info
           "No such state"
           {:state state
            :result-so-far acc}))))
     {:state :start}
     lines)))

(deftest document-parsing-test
  (is
   (=
    {:state :body,
     :head-comment-lines ["// this comment line is ignored"],
     :doctitle {:title "Document Title"},
     :author-line
     [{:author
       {:firstname "Kismet",
        :middlename "R.",
        :lastname "Lee",
        :authorinitials "KRL"},
       :email "kismet@asciidoctor.org"}],
     :document-attributes
     [{:attribute-name "description",
       :attribute-value "The document's description."}
      {:attribute-name "sectanchors", :attribute-value ""}
      {:attribute-name "url-repo",
       :attribute-value "https://my-git-repo.com"}],
     :current-block ["The document body starts here."]}

    (parse-document-lines (normalize (slurp (io/resource "juxt/reap/adoc_samples/example-1.adoc")))))))
