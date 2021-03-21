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
  (setq undercover-force-coverage t)
  (undercover "*.el" (:report-format 'simplecov)))

(require 'org-ol-tree)
(require 'buttercup)

(describe "Org Outline Tree helper functions"

  (it "It should allow to convert a section stack into a string"
    (expect (org-ol-tree--section-to-string '(4 3 2 1)) :to-equal "1.2.3.4"))

  (it "It should return nil if the given object is not a section stack"
    (expect (org-ol-tree--section-to-string '(4 "3" 2 1)) :to-be nil)
    (expect (org-ol-tree--section-to-string nil) :to-be nil)))

(provide 'test-helper-functions)
;;; test-section-functions.el ends here
