;;; undercover-init.el --- Initialization code for coverage information -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thiago Alves
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'undercover)
(setq undercover-force-coverage t)
(if (getenv "CODECOV_TOKEN")
    (undercover "org-ol-tree.el"
                (:report-file "./coverage-final.json")
                (:report-format 'codecov)
                (:send-report nil))
  (undercover "org-ol-tree.el"
              (:report-file nil)
              (:report-format 'text)))

;;; undercover-init.el ends here
