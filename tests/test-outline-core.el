;;; test-outline-core.el --- Sections and Headings tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thiago Alves
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file contains all tests related to the core objects (currently
;; 'sections' and 'headings').
;;
;; Since reading the test might be tedious, I'm reproducing here the transcript
;; of the behaviors being tested by this file:
;;
;; - Section
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
;; - Heading
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
;;   - when getting it from the outline buffer
;;     - should retrieve the heading by getting the :heading property of the button
;;
;; - Document
;;   - the object model (DOM)
;;     - retrieving the DOM from the Outline buffer
;;       - when retrieving an already created and clean DOM
;;         - should return the DOM without re-building it
;;       - when retrieving an already created DOM with the rebuild flag
;;         - should rebuild the DOM and return it
;;     - when retrieving the DOM for the first time with no rebuild flag
;;       - should rebuild the DOM and return it
;;     - when retrieving the DOM for the first time with the rebuild flag
;;       - should rebuild the DOM and return it
;;   - when creating from a non Org file
;;     - should fail with an user error
;;   - when creating from an Org file
;;     - should be able to traverse the document and return a heading DOM
;;   - when defining its name
;;     - from a file with no title
;;       - should use the file name in title-case as the node name
;;     - from a file with a defined title
;;       - should use the text defined on the '#+TITLE:' property as the node name
;;     - from an org buffer with no file and no title
;;       - should use the buffer name in title-case as the node name
;;
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'org-ol-tree)
(require 'buttercup)

(defconst test-root-dir (expand-file-name "tests"))


(describe "Section"
  (it "should be a list of integers or a string of integers separated by a '.'"
    (expect (org-ol-tree-core--section-p '(3 2 1)) :to-be-truthy)
    (expect (org-ol-tree-core--section-p nil) :not :to-be-truthy)
    (expect (org-ol-tree-core--section-p 42) :not :to-be-truthy)
    (expect (org-ol-tree-core--section-p '(3 "A" 1)) :not :to-be-truthy)
    (expect (org-ol-tree-core--section-p "1.2.3") :to-be-truthy)
    (expect (org-ol-tree-core--section-p "  ") :not :to-be-truthy)
    (expect (org-ol-tree-core--section-p "wrong") :not :to-be-truthy)
    (expect (org-ol-tree-core--section-p "1.B.3") :not :to-be-truthy))


  (describe "when converting between list and string"
    (it "should convert a section stack into a string"
      (expect (org-ol-tree-core--section-string '(4 3 2 1)) :to-equal "1.2.3.4"))

    (it "should return nil if the given object is not a section stack"
      (expect (org-ol-tree-core--section-string '(4 "3" 2 1)) :to-be nil)
      (expect (org-ol-tree-core--section-string 42) :to-be nil)
      (expect (org-ol-tree-core--section-string nil) :to-be nil))

    (it "should convert a string into a section stack"
      (expect (org-ol-tree-core--section-from-string "1.2.3.4") :to-equal '(4 3 2 1)))

    (it "should return nil if trying to convert a malformed string into a section stack"
      (expect (org-ol-tree-core--section-from-string "1.B.3.4") :to-be nil)
      (expect (org-ol-tree-core--section-from-string 42) :to-be nil)
      (expect (org-ol-tree-core--section-from-string nil) :to-be nil)))


  (describe "when calculating the next section"
    (it "should return the next section stack when creating a new section on a given level"
      (expect (org-ol-tree-core--next-section '() 3) :to-equal '(1 1 1))
      (expect (org-ol-tree-core--next-section '(3 2 1) 3) :to-equal '(4 2 1))
      (expect (org-ol-tree-core--next-section '(3 2 1) 2) :to-equal '(3 1))
      (expect (org-ol-tree-core--next-section '(3 2 1) 4) :to-equal '(1 3 2 1))
      (expect (org-ol-tree-core--next-section '(3 2 1) 10) :to-equal '(1 1 1 1 1 1 1 3 2 1))
      (expect (org-ol-tree-core--next-section "" 3) :to-equal '(1 1 1))
      (expect (org-ol-tree-core--next-section "1.2.3" 0) :to-equal '(2))
      (expect (org-ol-tree-core--next-section "1.2.3" 3) :to-equal '(4 2 1))
      (expect (org-ol-tree-core--next-section "1.2.3" 2) :to-equal '(3 1))
      (expect (org-ol-tree-core--next-section "1.2.3" 4) :to-equal '(1 3 2 1))
      (expect (org-ol-tree-core--next-section "1.2.3" 10) :to-equal '(1 1 1 1 1 1 1 3 2 1)))

    (it "should throw an exception when trying to get next section from a non-section object"
      (expect (org-ol-tree-core--next-section "  " 1) :to-throw)
      (expect (org-ol-tree-core--next-section "1.B.3" 1) :to-throw)
      (expect (org-ol-tree-core--next-section "1.1." 1) :to-throw)
      (expect (org-ol-tree-core--next-section 42 1) :to-throw)
      (expect (org-ol-tree-core--next-section '(3 "B" 1) 1) :to-throw)
      (expect (org-ol-tree-core--next-section '(3 "" 1) 1) :to-throw)
      (expect (org-ol-tree-core--next-section '(3 nil 1) 1) :to-throw))))



(describe "Heading"
  :var (empty-heading)

  (it "should allow me to create an object with no slot values"
    (expect (setq empty-heading (org-ol-tree-core--heading-create-internal)) :not :to-throw)
    (expect (org-ol-tree-core--heading-level empty-heading) :to-equal 0)
    (expect (listp (org-ol-tree-core--heading-subheadings empty-heading)) :to-be-truthy))


  (describe "when creating it from a non org buffer"
    (it "should throw a user error with a non-org buffer message"
      (expect (org-ol-tree-core--heading-create)
              :to-throw
              'user-error
              '("Cannot create an org-ol-tree-core--heading on a non-org buffer"))))


  (describe "when creating it from an org buffer"
    (before-each
      (org-mode))

    (after-each
      (org-mode))

    (it "should raise an error if cursor is not ona heading"
      (expect
       (org-ol-tree-core--heading-create)
       :to-throw
       'user-error
       '("Cannot create a heading with cursor outside an actual org headline")))


    (describe "when cursor is on a headline"
      :var ((heading-1 '(1 1 nil nil "Heading 1" nil))
            (heading-1-1 '(2 2 nil nil "Heading 1.1" nil))
            (heading-2 '(1 1 nil nil "Heading 2" nil))
            current-heading new-heading)

      (before-each
        (spy-on 'org-at-heading-p :and-return-value t))

      (it "should return a heading object for the section"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-core--heading-create))
        (expect (org-ol-tree-core--heading-name current-heading) :to-equal "Heading 1")
        (expect (org-ol-tree-core--heading-id current-heading) :to-equal "1"))

      (it "should throw an error if the giving previous heading is not a valid heading"
        (expect (org-ol-tree-core--heading-create 42)
                :to-throw 'error
                '("Given parent must be nil or an ’org-ol-tree-core--heading’ object")))

      (it "should return a sibling heading when passing previous heading on same level"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-core--heading-create))
        (spy-on 'org-heading-components :and-return-value heading-2)
        (setq new-heading (org-ol-tree-core--heading-create current-heading))
        (expect (org-ol-tree-core--heading-name new-heading) :to-equal "Heading 2")
        (expect (org-ol-tree-core--heading-id new-heading) :to-equal "2")
        (expect (org-ol-tree-core--heading-level new-heading) :to-equal 1))

      (it "should return a sub-heading when passing previous heading on lower level"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-core--heading-create))
        (spy-on 'org-heading-components :and-return-value heading-1-1)
        (setq new-heading (org-ol-tree-core--heading-create current-heading))
        (expect (org-ol-tree-core--heading-name new-heading) :to-equal "Heading 1.1")
        (expect (org-ol-tree-core--heading-id new-heading) :to-equal "1.1")
        (expect (org-ol-tree-core--heading-level new-heading) :to-equal 2))

      (it "should return a heading when passing previous heading on higher level"
        (spy-on 'org-heading-components :and-return-value heading-1)
        (setq current-heading (org-ol-tree-core--heading-create))
        (spy-on 'org-heading-components :and-return-value heading-1-1)
        (setq current-heading (org-ol-tree-core--heading-create current-heading))
        (spy-on 'org-heading-components :and-return-value heading-2)
        (setq new-heading (org-ol-tree-core--heading-create current-heading))
        (expect (org-ol-tree-core--heading-name new-heading) :to-equal "Heading 2")
        (expect (org-ol-tree-core--heading-id new-heading) :to-equal "2")
        (expect (org-ol-tree-core--heading-level new-heading) :to-equal 1))))


  (describe "when getting it from the outline buffer"
    :var (heading)

    (before-each
      (spy-on 'org-ol-tree-core--current-node :and-return-value 2)
      (spy-on 'get-text-property :and-return-value 'current-heading))

    (it "should retrieve the heading by getting the :heading property of the button"
      (setq heading (org-ol-tree-core--heading-current))

      (expect 'org-ol-tree-core--current-node :to-have-been-called-times 1)
      (expect 'get-text-property :to-have-been-called-with 2 :heading)
      (expect heading :to-be 'current-heading))))



(describe "Document"

  (describe "the object model (DOM)"

    (before-each
      (spy-on 'org-ol-tree-core--doc-create :and-return-value 'MyGeneratedDOM))


    (describe "retrieving the DOM from the Outline buffer"
      :var (org-ol-tree--buffer-p
            org-ol-tree--org-buffer)

      (before-each
        (setq-local org-ol-tree--buffer-p t
                    org-ol-tree--org-buffer 'OrgBuffer))


      (describe "when retrieving an already created and clean DOM"
        :var (org-ol-tree-core--DOM org-ol-tree-core--rebuild-DOM-p)

        (before-each
          (setq-local org-ol-tree-core--DOM 'MyDOM
                      org-ol-tree-core--rebuild-DOM-p nil))

        (it "should return the DOM without re-building it"
          (expect (org-ol-tree-core--doc) :to-be 'MyDOM)
          (expect 'org-ol-tree-core--doc-create :not :to-have-been-called)))


      (describe "when retrieving an already created DOM with the rebuild flag"
        :var (org-ol-tree-core--DOM org-ol-tree-core--rebuild-DOM-p)

        (before-each
          (setq-local org-ol-tree-core--DOM 'MyDOM
                      org-ol-tree-core--rebuild-DOM-p t))

        (it "should rebuild the DOM and return it"
          (expect (org-ol-tree-core--doc) :to-be 'MyGeneratedDOM)
          (expect 'org-ol-tree-core--doc-create :to-have-been-called-with 'OrgBuffer))))


    (describe "when retrieving the DOM for the first time with no rebuild flag"
      :var (org-ol-tree-core--DOM org-ol-tree-core--rebuild-DOM-p)

      (before-each
        (setq-local org-ol-tree-core--DOM nil
                    org-ol-tree-core--rebuild-DOM-p nil))

      (it "should rebuild the DOM and return it"
        (expect (org-ol-tree-core--doc) :to-be 'MyGeneratedDOM)
        (expect 'org-ol-tree-core--doc-create :to-have-been-called-with 'OrgBuffer)))


    (describe "when retrieving the DOM for the first time with the rebuild flag"
      :var (org-ol-tree-core--DOM org-ol-tree-core--rebuild-DOM-p)

      (before-each
        (setq-local org-ol-tree-core--DOM nil
                    org-ol-tree-core--rebuild-DOM-p t))

      (it "should rebuild the DOM and return it"
        (expect (org-ol-tree-core--doc) :to-be 'MyGeneratedDOM)
        (expect 'org-ol-tree-core--doc-create :to-have-been-called-with 'OrgBuffer))))


  (describe "when creating from a non Org file"
    (it "should fail with an user error"
      (with-temp-buffer
        (expect (org-ol-tree-core--doc-create (current-buffer))
                :to-throw 'user-error '("Can’t traverse an Org document on a NON-Org buffer")))))


  (describe "when creating from an Org file"
    :var (doc-dom)

    (it "should be able to traverse the document and return a heading DOM"
      (find-file (expand-file-name "data/doc-happy-path.org" test-root-dir))
      (setq doc-dom (org-ol-tree-core--doc-create (current-buffer)))

      (expect doc-dom :not :to-be nil)
      (expect (length (org-ol-tree-core--heading-subheadings doc-dom)) :to-equal 3)

      (kill-buffer (current-buffer))))


  (describe "when defining its name"

    (describe "from a file with no title"

      (before-each
        (find-file (expand-file-name "data/doc-no-title.org" test-root-dir)))

      (after-each
        (kill-buffer (current-buffer)))

      (it "should use the file name in title-case as the node name"
        (expect (org-ol-tree-core--doc-name) :to-equal "Doc No Title")))


    (describe "from a file with a defined title"

      (before-each
        (find-file (expand-file-name "data/doc-happy-path.org" test-root-dir)))

      (after-each
        (kill-buffer (current-buffer)))

      (it "should use the text defined on the '#+TITLE:' property as the node name"
        (expect (org-ol-tree-core--doc-name) :to-equal "Document Title")))


    (describe "from an org buffer with no file and no title"

      (it "should use the buffer name in title-case as the node name"

        (expect (with-current-buffer (create-file-buffer "my-non-file-org-buffer")
                  (org-ol-tree-core--doc-name))
                :to-equal
                "My Non File Org Buffer")))))



(provide 'test-outline-core)
;;; test-outline-core.el ends here
