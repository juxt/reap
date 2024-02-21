;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.adoc-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.regex :as re :refer [input]]
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

;; Therefore, we define a line as this regex pattern: ^.*(?:\R|\z)
;; Note that \R is chosen rather than \n, for seamless operation on
;; Windows/MacOS (in slight deviation from Asciidoctor)

(def comment-line
  (p/pattern-parser #"//.*(?:\R|\z)" {}))

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
   (p/pattern-parser #"=\h+(.*)(?:\R|\z)" {:group {:doctitle 1}})))

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
    (p/ignore (p/pattern-parser #"(?:\R|\z)")))))

(defn trim-vals [m]
  (update-vals m str/trim))

(def revision-line
  (p/comp
   trim-vals
   (p/alternatives
    (p/pattern-parser
     #"^v?[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)\h*\,\h*(?<RevisionDate>[\p{Alnum}-,\h]+)\h*\:\h*(?<Remark>.*)(?:\R|\z)"
     {:group
      {:revision-number "RevisionNumber"
       :revision-date "RevisionDate"
       :revision-remark "Remark"}})

    ;; "When the revision line contains a version and a date, separate
    ;; the version number from the date with a comma"
    (p/pattern-parser
     #"v?[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)\h*(?:,\h*(?<RevisionDate>[\p{Alnum}\-,\h]+))(?:\R|\z)"
     {:group
      {:revision-number "RevisionNumber"
       :revision-date "RevisionDate"}})

    (p/pattern-parser
     #"v?[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)\h*(?:\:\h*(?<Remark>.*))?(?:\R|\z)"
     {:group
      {:revision-number "RevisionNumber"
       :revision-remark "Remark"}})

    ;; "When the revision line only contains a revision number, prefix
    ;; the number with a v." --
    ;; https://docs.asciidoctor.org/asciidoc/latest/document/revision-line/
    (p/pattern-parser
     #"v[^\p{Digit}\:]*?(?<RevisionNumber>[\p{Digit}\.]+)(?:\R|\z)"
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
     #"^\:([^:]+)\:\h+(\S.*)(?:\R|\z)"
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
     #"^\:([^:]+)\:\h*(?:\R|\z)"
     {:group
      {:attribute-label 1}}))))

(comment
  (attribute-entry (input ":name-of-an-attribute: value of the attribute\n")))

(def header
  (p/as-map
   (p/cons
    ;; Comment lines are optional
    (p/as-entry
     :comment-lines
     (p/comp
      (fn [lines] (mapv str/trim lines))
      (p/one-or-more comment-line)))
    (p/cons
     (p/as-entry :doctitle doctitle)
     (p/alternatives
      (p/sequence-group
       (p/as-entry :author-line author-line)
       (p/as-entry :revision-line revision-line)
       (p/as-entry :attributes (p/comp vec (p/zero-or-more attribute-entry)))
       (p/ignore (p/pattern-parser #"(?:\R|\z)")))
      (p/sequence-group
       (p/as-entry :author-line author-line)
       (p/as-entry :attributes (p/comp vec (p/zero-or-more attribute-entry)))
       (p/ignore (p/pattern-parser #"(?:\R|\z)")))
      (p/sequence-group
       (p/as-entry :attributes (p/comp vec (p/zero-or-more attribute-entry)))
       (p/ignore (p/pattern-parser #"(?:\R|\z)"))))))))

#_(def document
    (p/complete
     (p/sequence-group
      (p/optionally header)
      (p/optionally preamble)
      (p/zero-or-more section))))

(deftest parser-test
  (testing "document title"
    (is (doctitle (input "= The Intrepid Chronicles\nfoo")))
    (is (not (doctitle (input "The Intrepid Chronicles"))))
    (is
     (= {:title "foo"}
        (doctitle (input "= foo"))))
    (is
     (= {:title "foo"
         :subtitle "bar"}
        (doctitle (input "= foo: bar"))))
    (is
     (= {:title "foo: bar" :subtitle "zip"}
        (doctitle (input "= foo: bar: zip")))))

  (testing "author"
    (is (author (input "Malcolm")))
    (is (author (input "Malcolm Sparks")))
    (is (author (input "Malcolm James Sparks"))))

  (testing "compound names in the author line"
    (is
     (=
      [{:author {:firstname "Ann Marie" :lastname "Jenson" :authorinitials "AJ"}}
       {:author {:firstname "Tomás" :lastname "López del Toro" :authorinitials "TL"}}]
      (author-line (input "Ann_Marie Jenson; Tomás López_del_Toro")))))

  (testing "email"
    (is (email (input "mal@juxt.pro")))
    (is (email (input "MAL@JUXT.PRO")))
    (is (not (email (input "mal")))))

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
       (input
        "Kismet R. Lee <kismet@asciidoctor.org>; B. Steppenwolf; Pax Draeke <pax@asciidoctor.org>")))))

  (testing "revision line"
    (is (= {:revision-number "7.5"}
           (revision-line (input "v7.5"))))
    (is (= {:revision-number "7.5" :revision-date "1-29-2020"}
           (revision-line (input "7.5, 1-29-2020"))))
    (is (= {:revision-number "7.5" :revision-date "1-29-2020"}
           (revision-line (input "v7.5, 1-29-2020"))))
    (is (= {:revision-number "7.5"
            :revision-remark "A new analysis"}
           (revision-line (input "7.5: A new analysis"))))
    (is (= {:revision-number "7.5"
            :revision-remark "A new analysis"}
           (revision-line (input "v7.5: A new analysis"))))
    (is (= {:revision-number "7.5"
            :revision-date "1-29-2020"
            :revision-remark "A new analysis"}
           (revision-line (input "7.5, 1-29-2020: A new analysis"))))
    (is (= {:revision-number "7.5"
            :revision-date "1-29-2020"
            :revision-remark "A new analysis"}
           (revision-line (input "v7.5, 1-29-2020: A new analysis")))))

  (testing "attributes"
    (is (= {:attribute-name "name-of-an-attribute"
            :attribute-value ""}
           (attribute-entry (input ":name-of-an-attribute:  \n"))))
    (is (= {:attribute-name "name-of-an-attribute"
            :attribute-value "value of the attribute"}
           (attribute-entry (input ":name-of-an-attribute: value of the attribute  \n"))))
    (is (= {:attribute-name "name-of-an-attribute"
            :attribute-value nil}
           (attribute-entry (input ":name-of-an-attribute!: \n"))))
    (is (not (attribute-entry (input ":foo:bar")))))

  (testing "header"
    (is
     (=
      {:doctitle {:title "Document Title"}
       :author-line
       [{:author {:firstname "Author"
                  :lastname "Name"
                  :authorinitials "AN"}
         :email "author@email.org"}]
       :attributes []}
      (header (input "= Document Title\nAuthor Name <author@email.org>"))))

    (is
     (= {:doctitle {:title "The Intrepid Chronicles"},
         :author-line
         [{:author
           {:firstname "Kismet", :lastname "Lee", :authorinitials "KL"}}],
         :revision-line
         {:revision-number "2.9",
          :revision-date "October 31, 2021",
          :revision-remark "Fall incarnation"}
         :attributes [{:attribute-name "sectnums", :attribute-value ""}
                      {:attribute-name "toclevels", :attribute-value "3"}]}
        (header (input "= The Intrepid Chronicles\nKismet Lee\n2.9, October 31, 2021: Fall incarnation\n:sectnums:\n:toclevels: 3"))))

    (is
     (= {:comment-lines ["// this comment line is ignored"]
         :doctitle {:title "Document Title"}
         :author-line
         [{:author
           {:firstname "Kismet"
            :middlename "R."
            :lastname "Lee"
            :authorinitials "KRL"}
           :email "kismet@asciidoctor.org"}]
         :attributes
         [{:attribute-name "description"
           :attribute-value "The document's description."}
          {:attribute-name "sectanchors"
           :attribute-value ""}
          {:attribute-name "url-repo"
           :attribute-value "https://my-git-repo.com"}]}
        (header
         (input (slurp (io/resource "juxt/reap/adoc_samples/example-1.adoc"))))))

    (is
     (=
      {:doctitle {:title "The Intrepid Chronicles"}, :attributes []}
      (header
       (input (slurp (io/resource "juxt/reap/adoc_samples/example-2.adoc"))))))))

;; TODO: Escape a trailing character reference (https://docs.asciidoctor.org/asciidoc/latest/document/multiple-authors/)
;; TODO: Assign Author and Email with Attribute Entries (https://docs.asciidoctor.org/asciidoc/latest/document/author-attribute-entries/)
;; TODO: Reference the Author Information (https://docs.asciidoctor.org/asciidoc/latest/document/reference-author-attributes/)
