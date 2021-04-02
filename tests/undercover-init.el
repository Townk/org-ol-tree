;;; undercover-init.el --- Initialization code for coverage information -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thiago Alves
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(if (getenv "CODECOV_TOKEN")
    (when (require 'undercover nil t)
      (setq undercover-force-coverage t)
      (undercover "org-ol-tree.el"
                  (:report-format 'codecov)
                  (:send-report nil)))
  (when (require 'undercover nil t)
    (mkdir "coverage" t)
    (setq undercover-force-coverage t)
    (undercover "org-ol-tree.el"
                (:report-format 'simplecov)
                (:send-report nil))))

;;; undercover-init.el ends here
