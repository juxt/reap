;; Copyright © 2024, JUXT LTD.

(ns juxt.reap.adoc-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.reap.regex :as re :refer [input]]
   [juxt.reap.combinators :as p]
   [clojure.string :as str]))

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

(def title
  (p/comp
   (fn [{:keys [title]}]
     (let [segments
           ;; TODO: See
           ;; https://docs.asciidoctor.org/asciidoc/latest/document/subtitle/ for
           ;; discussion about assigning a custom separator to the document
           ;; title. Not yet implemented.
           (str/split title #":\s")]
       (if (< (count segments) 2)
         {:title (first segments)}
         {:title (str/join ": " (butlast segments))
          :subtitle (last segments)})))
   (p/pattern-parser #"=\s+(.*)" {:group {:title 1}})))

(def firstname (p/pattern-parser #"[\p{Alpha}\.\']+"))
(def middlename (p/pattern-parser #"[\p{Alpha}\.\']+"))
(def lastname (p/pattern-parser #"[\p{Alpha}\.\']+"))

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
   (p/into
    {}
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
         (p/ignore (p/pattern-parser #"\s+"))
         middlename)))
      (p/as-entry
       :lastname
       (p/first
        (p/sequence-group
         (p/ignore (p/pattern-parser #"\s+"))
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
         (p/ignore (p/pattern-parser #"\s+"))
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
  (p/into
   {}
   (p/sequence-group
    (p/as-entry
     :author
     author)
    (p/optionally
     (p/as-entry
      :email
      (p/first
       (p/sequence-group
        (p/ignore (p/pattern-parser #"\s+"))
        email-with-angle-brackets)))))))

(def author-infos
  (p/comp
   vec
   (p/cons
    author-info
    (p/zero-or-more
     (p/first
      (p/sequence-group
       (p/ignore (p/pattern-parser #";\s+"))
       author-info))))))

(def header
  (p/into
   {}
   (p/alternatives
    (p/sequence-group
     (p/as-entry :doctitle title)
     (p/ignore
      (p/pattern-parser #"\n"))
     (p/as-entry :author-infos author-infos))
    (p/sequence-group
     (p/as-entry :title title)))))

#_(def document
    (p/complete
     (p/sequence-group
      (p/optionally header)
      (p/optionally preamble)
      (p/zero-or-more section))))

(deftest parser-test
  (testing "document title"
    (is (title (input "= The Intrepid Chronicles\nfoo")))
    (is (not (title (input "The Intrepid Chronicles"))))
    (is
     (= {:title "foo"}
        (title (input "= foo"))))
    (is
     (= {:title "foo"
         :subtitle "bar"}
        (title (input "= foo: bar"))))
    (is
     (= {:title "foo: bar" :subtitle "zip"}
        (title (input "= foo: bar: zip")))))
  (testing "author"
    (is (author (input "Malcolm")))
    (is (author (input "Malcolm Sparks")))
    (is (author (input "Malcolm James Sparks"))))
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
      (author-infos
       (input
        "Kismet R. Lee <kismet@asciidoctor.org>; B. Steppenwolf; Pax Draeke <pax@asciidoctor.org>")))))
  (testing "header"
    (is
     (=
      {:doctitle {:title "Document Title"}
       :author-infos
       [{:author {:firstname "Author"
                  :lastname "Name"
                  :authorinitials "AN"}
         :email "author@email.org"}]}
      (header (input "= Document Title\nAuthor Name <author@email.org>"))))))

;; TODO: Escape a trailing character reference (https://docs.asciidoctor.org/asciidoc/latest/document/multiple-authors/)
;; TODO: Assign Author and Email with Attribute Entries (https://docs.asciidoctor.org/asciidoc/latest/document/author-attribute-entries/)
;; TODO: Reference the Author Information (https://docs.asciidoctor.org/asciidoc/latest/document/reference-author-attributes/)
;; TODO: Compound Author Names (https://docs.asciidoctor.org/asciidoc/latest/document/compound-author-name/)