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

(describe "Org Outline Tree sections helper functions"

  (it "should convert a section stack into a string"
    (expect (org-ol-tree--section-to-string '(4 3 2 1)) :to-equal "1.2.3.4"))

  (it "should return nil if the given object is not a section stack"
    (expect (org-ol-tree--section-to-string '(4 "3" 2 1)) :to-be nil)
    (expect (org-ol-tree--section-to-string 42) :to-be nil)
    (expect (org-ol-tree--section-to-string nil) :to-be nil))

  (it "should convert a string into a section stack"
    (expect (org-ol-tree--section-to-stack "1.2.3.4") :to-equal '(4 3 2 1)))

  (it "should return nil if trying to convert a malformed string into a section stack"
    (expect (org-ol-tree--section-to-stack "1.B.3.4") :to-be nil)
    (expect (org-ol-tree--section-to-stack 42) :to-be nil)
    (expect (org-ol-tree--section-to-stack nil) :to-be nil))

  (it "should return the next section stack when creating a new section on a given level"
    (expect (org-ol-tree--next-section '(3 2 1) 3) :to-equal '(4 2 1))
    (expect (org-ol-tree--next-section '(3 2 1) 2) :to-equal '(3 1))
    (expect (org-ol-tree--next-section '(3 2 1) 4) :to-equal '(1 3 2 1))))

(provide 'test-helper-functions)
;;; test-section-functions.el ends here
