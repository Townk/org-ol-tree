;;; test-outline-core.el --- Sections and Headings tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thiago Alves
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file contains all tests related to the outline elements (currently
;; 'sections' and 'headings').
;;
;; Since reading the test might be tedious, I'm reproducing here the transcript
;; of the behaviors being tested by this file:
;;
;; - A section object
;;   - should be a list of integers or a string of integers separated by a '.'
;;   - when converting between list and string
;;     - should convert a section stack into a string
;;     - should return nil if the given object is not a section stack
;;     - should convert a string into a section stack
;;     - should return nil if trying to convert a malformed string into a section stack
;;   - when calculating the next section
;;     - should return the next section stack when creating a new section on a given level
;;     - should throw an exception when trying to get next section from a non-section object
;;
;; - A heading structure
;;   - should allow me to create an object with no slot values
;;   - when creating it from a non org buffer
;;     - should throw a user error with a non-org buffer message
;;   - when creating it from an org buffer
;;     - should raise an error if cursor is not ona heading
;;     - when cursor is on a headline
;;       - should return a heading object for the section
;;       - should throw an error if the giving previous heading is not a valid heading
;;       - should return a sibling heading when passing previous heading on same level
;;       - should return a sub-heading when passing previous heading on lower level
;;       - should return a heading when passing previous heading on higher level
;;
;;; Code:

(when (require 'undercover nil t)
  (undercover "org-ol-tree.el"))

(require 'org-ol-tree)
(require 'buttercup)

(defconst test-root-dir (expand-file-name "tests"))

(describe "Section object"
  (it "should be a list of integers or a string of integers separated by a '.'"
    (expect (org-ol-tree-section-p '(3 2 1)) :to-be-truthy)
    (expect (org-ol-tree-section-p nil) :not :to-be-truthy)
    (expect (org-ol-tree-section-p 42) :not :to-be-truthy)
    (expect (org-ol-tree-section-p '(3 "A" 1)) :not :to-be-truthy)
    (expect (org-ol-tree-section-p "1.2.3") :to-be-truthy)
    (expect (org-ol-tree-section-p "  ") :not :to-be-truthy)
    (expect (org-ol-tree-section-p "wrong") :not :to-be-truthy)
    (expect (org-ol-tree-section-p "1.B.3") :not :to-be-truthy))


  (describe "when converting between list and string"
    (it "should convert a section stack into a string"
      (expect (org-ol-tree-section-stack-to-string '(4 3 2 1)) :to-equal "1.2.3.4"))

    (it "should return nil if the given object is not a section stack"
      (expect (org-ol-tree-section-stack-to-string '(4 "3" 2 1)) :to-be nil)
      (expect (org-ol-tree-section-stack-to-string 42) :to-be nil)
      (expect (org-ol-tree-section-stack-to-string nil) :to-be nil))

    (it "should convert a string into a section stack"
      (expect (org-ol-tree-section-string-to-stack "1.2.3.4") :to-equal '(4 3 2 1)))

    (it "should return nil if trying to convert a malformed string into a section stack"
      (expect (org-ol-tree-section-string-to-stack "1.B.3.4") :to-be nil)
      (expect (org-ol-tree-section-string-to-stack 42) :to-be nil)
      (expect (org-ol-tree-section-string-to-stack nil) :to-be nil)))


  (describe "when calculating the next section"
    (it "should return the next section stack when creating a new section on a given level"
      (expect (org-ol-tree-section-next '() 3) :to-equal '(1 1 1))
      (expect (org-ol-tree-section-next '(3 2 1) 3) :to-equal '(4 2 1))
      (expect (org-ol-tree-section-next '(3 2 1) 2) :to-equal '(3 1))
      (expect (org-ol-tree-section-next '(3 2 1) 4) :to-equal '(1 3 2 1))
      (expect (org-ol-tree-section-next '(3 2 1) 10) :to-equal '(1 1 1 1 1 1 1 3 2 1))
      (expect (org-ol-tree-section-next "" 3) :to-equal '(1 1 1))
      (expect (org-ol-tree-section-next "1.2.3" 0) :to-equal '(2))
      (expect (org-ol-tree-section-next "1.2.3" 3) :to-equal '(4 2 1))
      (expect (org-ol-tree-section-next "1.2.3" 2) :to-equal '(3 1))
      (expect (org-ol-tree-section-next "1.2.3" 4) :to-equal '(1 3 2 1))
      (expect (org-ol-tree-section-next "1.2.3" 10) :to-equal '(1 1 1 1 1 1 1 3 2 1)))

    (it "should throw an exception when trying to get next section from a non-section object"
      (expect (org-ol-tree-section-next "  " 1) :to-throw)
      (expect (org-ol-tree-section-next "1.B.3" 1) :to-throw)
      (expect (org-ol-tree-section-next "1.1." 1) :to-throw)
      (expect (org-ol-tree-section-next 42 1) :to-throw)
      (expect (org-ol-tree-section-next '(3 "B" 1) 1) :to-throw)
      (expect (org-ol-tree-section-next '(3 "" 1) 1) :to-throw)
      (expect (org-ol-tree-section-next '(3 nil 1) 1) :to-throw))))


(describe "Heading structure"
  :var (empty-heading)

  (it "should allow me to create an object with no slot values"
    (expect (setq empty-heading (org-ol-tree-heading--create)) :not :to-throw)
    (expect (org-ol-tree-heading-level empty-heading) :to-equal 0)
    (expect (listp (org-ol-tree-heading-subheadings empty-heading)) :to-be-truthy))


  (describe "when creating it from a non org buffer"
    (it "should throw a user error with a non-org buffer message"
      (expect (org-ol-tree-heading-create)
              :to-throw 'user-error '("Cannot create an org-ol-tree-heading on a non-org buffer"))))


  (describe "when creating it from an org buffer"
    (before-each
      (org-mode))

    (after-each
      (org-mode))

    (it "should raise an error if cursor is not ona heading"
      (expect
       (org-ol-tree-heading-create)
              :to-throw 'user-error
              '("Cannot create an org-ol-tree-heading with cursor outside an actual org headline")))


    (describe "when cursor is on a headline"
      :var ((heading-1 '(1 1 nil nil "Heading 1" nil))
            (heading-1-1 '(2 2 nil nil "Heading 1.1" nil))
            (heading-2 '(1 1 nil nil "Heading 2" nil))
            current-heading new-heading)

      (before-each
        (spy-on 'org-at-heading-p :and-return-value t))

      (it "should return a heading object for the section"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-heading-create))
        (expect (org-ol-tree-heading-name current-heading) :to-equal "Heading 1")
        (expect (org-ol-tree-heading-id current-heading) :to-equal "1"))

      (it "should throw an error if the giving previous heading is not a valid heading"
        (expect (org-ol-tree-heading-create 42)
                :to-throw 'error
                '("Given parent must be nil or an ’org-ol-tree-heading’ object")))

      (it "should return a sibling heading when passing previous heading on same level"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-heading-create))
        (spy-on 'org-heading-components :and-return-value heading-2)
        (setq new-heading (org-ol-tree-heading-create current-heading))
        (expect (org-ol-tree-heading-name new-heading) :to-equal "Heading 2")
        (expect (org-ol-tree-heading-id new-heading) :to-equal "2")
        (expect (org-ol-tree-heading-level new-heading) :to-equal 1))

      (it "should return a sub-heading when passing previous heading on lower level"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-heading-create))
        (spy-on 'org-heading-components :and-return-value heading-1-1)
        (setq new-heading (org-ol-tree-heading-create current-heading))
        (expect (org-ol-tree-heading-name new-heading) :to-equal "Heading 1.1")
        (expect (org-ol-tree-heading-id new-heading) :to-equal "1.1")
        (expect (org-ol-tree-heading-level new-heading) :to-equal 2))

      (it "should return a heading when passing previous heading on higher level"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-heading-create))
        (spy-on 'org-heading-components :and-return-value heading-1-1)
        (setq current-heading (org-ol-tree-heading-create current-heading))
        (spy-on 'org-heading-components :and-return-value heading-2)
        (setq new-heading (org-ol-tree-heading-create current-heading))
        (expect (org-ol-tree-heading-name new-heading) :to-equal "Heading 2")
        (expect (org-ol-tree-heading-id new-heading) :to-equal "2")
        (expect (org-ol-tree-heading-level new-heading) :to-equal 1)))))


(describe "Document traversal"
  (describe "When requested to traverse a non Org file"
    (it "should fail with an user error"
      (with-temp-buffer
        (expect (org-ol-tree-heading-root-build (current-buffer))
                :to-throw 'user-error '("Can’t traverse an Org document on a NON-Org buffer")))))

  (describe "When requested to traverse an Org file"
    :var (doc-dom)

    (it "should be able to traverse the document and return a heading DOM"
      (find-file (expand-file-name "data/doc-happy-path.org" test-root-dir))
      (setq doc-dom (org-ol-tree-heading-root-build (current-buffer)))

      (expect doc-dom :not :to-be nil)
      (expect (length (org-ol-tree-heading-subheadings doc-dom)) :to-equal 3)

      (kill-buffer (current-buffer)))))


(describe "Root node name"
  (describe "on a file with no title"

    (before-each
      (find-file (expand-file-name "data/doc-no-title.org" test-root-dir)))

    (after-each
      (kill-buffer (current-buffer)))

    (it "should use the file name in title-case as the node name"
      (expect (org-ol-tree-heading-root-label) :to-equal "Doc No Title")))

  (describe "on a file with a defined title"

    (before-each
      (find-file (expand-file-name "data/doc-happy-path.org" test-root-dir)))

    (after-each
      (kill-buffer (current-buffer)))

    (it "should use the text defined on the '#+TITLE:' property as the node name"
      (expect (org-ol-tree-heading-root-label) :to-equal "Document Title")))

  (describe "on an org buffer with no file and no title"

    (it "should use the buffer name in title-case as the node name"

      (expect (with-current-buffer (create-file-buffer "my-non-file-org-buffer")
                (org-ol-tree-heading-root-label))
              :to-equal
              "My Non File Org Buffer")))
  )

(provide 'test-outline-core)
;;; test-outline-core.el ends here
