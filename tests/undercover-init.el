
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
