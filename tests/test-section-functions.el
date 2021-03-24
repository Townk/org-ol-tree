;;; test-section-functions.el --- tests for org-ol-tree section functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thiago Alves
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;; Code:

(when (require 'undercover nil t)
  (undercover "org-ol-tree.el"))

(require 'org-ol-tree)
(require 'buttercup)

(describe "A Section"
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

(describe "A heading structure"
  (it "should allow me to create an object with no slot values"
    (expect (org-ol-tree-heading--create) :not :to-throw))

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
        (expect (org-ol-tree-heading-level new-heading) :to-equal 1)))
    ))

(provide 'test-helper-functions)
;;; test-section-functions.el ends here
