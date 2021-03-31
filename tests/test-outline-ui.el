;;; test-outline-ui.el --- Sections and Headings tests -*- lexical-binding: t; -*-
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
;; - Icons:
;;   - Given the list of available icon sets,
;;     - expect each set to contain only :root, :expanded, :collapsed, and :section icons
;;     - when refreshing it,
;;       - it should use the set defined on var `org-ol-tree-ui-icon-set' when defined
;;       - it should use the `all-the-icons' set if all-the-icons is available and on GUI
;;       - it should use the `unicode' set if on GUI but without all-the-icons installed
;;       - it should use the `ascii' set if not on GUI
;;   - Given a document icon,
;;     - when it is nil in the icon set,
;;       - it should have a single white space representing the cursor column
;;     - when the icon set has an empty icon for it,
;;       - it should have a single white space representing the cursor column
;;     - when the icon set has an actual icon for it,
;;       - it should have a white space in the beginning and one in the end of the icon string
;;   - Given a section icon,
;;     - when the section used to get the icon has subsections under it,
;;       - and the section state is 'expanded,
;;         - it should display the cursor column plus the expanded icon on its beginning
;;       - and the section state is 'collapsed,
;;         - it should display the cursor column plus the collapsed icon on its beginning
;;     - when the section used to get the icon does not have subsections under it,
;;       - it should start with a cursor column space plus two spaces for alignment
;;     - when the section icon contains the %(section) tag
;;       - it should show the section id on the icon
;;     - when the section icon does not contain the %(section) tag
;;       - it should show the section id on the icon
;;
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'org-ol-tree)
(require 'buttercup)
(require 's)
(require 'seq)

(defconst test-root-dir (expand-file-name "tests"))

(defun tests-text-expr-and-value-no-property (fun)
  "Alternative implementation of `buttercup--expr-and-value'.

As with `buttercup--expr-and-value', given FUN, return its quoted expression and
value. The difference here, is the value returned will get all its text
properties removed with the `substring-no-properties' function."
  (cons (buttercup--enclosed-expr fun)
        (substring-no-properties (funcall fun))))


(buttercup-define-matcher :to-start-with (text prefix &optional ignore-case)
  (cl-destructuring-bind
      ((text-expr . text) (prefix-expr . prefix))
      (mapcar #'tests-text-expr-and-value-no-property (list text prefix))
    (let* ((ignore-case-p (when ignore-case (funcall ignore-case)))
           (actual-start (s-left (length prefix) text))
           (matches-p (s-starts-with-p prefix text ignore-case-p))
           (spec (format-spec-make
                  ?T (format "%S" text-expr)
                  ?e (format "%S" prefix-expr)
                  ?a (format "%S" actual-start))))
      (cond
       ((and ignore-case-p matches-p)
        (cons t (buttercup-format-spec
                 "Expected `%T' to ignore case and not to start with `%e', but it did."
                 spec)))
       ((and (not ignore-case-p) matches-p)
        (cons t (buttercup-format-spec
                 "Expected `%T' not to start exactly with `%e', but it did."
                 spec)))
       ((and ignore-case-p (not matches-p))
        (cons nil (buttercup-format-spec
                   (concat
                    "Expected `%T' to start with `%e' ignoring case, "
                    "but `%a' was in the beginning of the text.")
                   spec)))
       (t (cons nil (buttercup-format-spec
                     (concat
                      "Expected `%T' to start exactly with `%e', "
                      "but `%a' was in the beginning of the text.")
                 spec)))))))


(buttercup-define-matcher :to-end-with (text sufix &optional ignore-case)
  (cl-destructuring-bind
      ((text-expr . text) (sufix-expr . sufix))
      (mapcar #'tests-text-expr-and-value-no-property (list text sufix))
    (let* ((ignore-case-p (when ignore-case (funcall ignore-case)))
           (actual-end (s-right (length sufix) text))
           (matches-p (s-ends-with-p sufix text ignore-case-p))
           (spec (format-spec-make
                  ?T (format "%S" text-expr)
                  ?e (format "%S" sufix-expr)
                  ?a (format "%S" actual-end))))
      (cond
       ((and ignore-case-p matches-p)
        (cons t (buttercup-format-spec
                 "Expected `%T' to ignore case and not to end with `%e', but it did."
                 spec)))
       ((and (not ignore-case-p) matches-p)
        (cons t (buttercup-format-spec
                 "Expected `%T' not to end exactly with `%e', but it did."
                 spec)))
       ((and ignore-case-p (not matches-p))
        (cons nil (buttercup-format-spec
                   (concat
                    "Expected `%T' to end with `%e' ignoring case, "
                    "but `%a' was in the beginning of the text.")
                   spec)))
       (t (cons nil (buttercup-format-spec
                     (concat
                      "Expected `%T' to end exactly with `%e', "
                      "but `%a' was in the beginning of the text.")
                 spec)))))))


(buttercup-define-matcher :to-have-length-greater-than (text length)
  (cl-destructuring-bind
      ((text-expr . text) (length-expr . length))
      (mapcar #'buttercup--expr-and-value (list text length))
    (let* ((length-actual (length text))
           (spec (format-spec-make
                  ?T (format "%S" text-expr)
                  ?l (format "%S" length-expr)
                  ?a (format "%S" length-actual))))
      (if (> (length text) length)
          (cons t (buttercup-format-spec
                   "Expected `%T' to have a length of at most `%l', but the length was `%a'."
                   spec))
        (cons nil (buttercup-format-spec
                   "Expected `%T' to have a length greater than `%l', but the length was `%a'."
                   spec))))))


(buttercup-define-matcher :to-have-the-exact-keys (plist keys)
  (cl-destructuring-bind
      ((plist-expr . plist) (keys-expr . keys))
      (mapcar #'buttercup--expr-and-value (list plist keys))
      (let* ((plist-unique-keys (list))
             (keys-unique-keys (list))
             spec)
        (cl-loop for (key _) on plist by (function cddr) do
                 (unless (member key keys)
                   (push key plist-unique-keys)))
        (cl-loop for (key) on keys do
                 (unless (plist-member plist key)
                   (push key keys-unique-keys)))
        (setq spec (format-spec-make
                    ?L (format "%S" plist-expr)
                    ?l (format "%S" plist)
                    ?K (format "%S" keys-expr)
                    ?k (format "%S" keys)
                    ?p (format "%S" plist-unique-keys)
                    ?y (format "%S" keys-unique-keys)))
        (cond
         ((and plist-unique-keys keys-unique-keys)
          (cons nil (buttercup-format-spec
                     "Expected `%L' to contain only the keys `%k', but `%y' are missing and `%p' are present unexpectedly."
                     spec)))
         (plist-unique-keys
          (cons nil (buttercup-format-spec
                     "Expected `%L' to contain only the keys `%k', but `%p' are present unexpectedly."
                     spec)))
         (keys-unique-keys
          (cons nil (buttercup-format-spec
                     "Expected `%L' to contain only the keys `%k', but `%y' are missing."
                     spec)))
         (t
          (cons t (buttercup-format-spec
                   "Expected `%L' not to have none of the keys `%k'"
                   spec)))))))


(describe "Icons:"

  (describe "Given the list of available icon sets,"

    (it "expect each set to contain only :root, :expanded, :collapsed, and :section icons"
      (cl-loop for (_ icon-set) on org-ol-tree-ui-icon-set-list by (function cddr) do
               (expect icon-set :to-have-the-exact-keys '(:root :expanded :collapsed :section))))

    (describe "when refreshing it,"
      :var (org-ol-tree-ui-icon-set)

      (it "it should use the set defined on var `org-ol-tree-ui-icon-set' when defined"
        (setq org-ol-tree-ui-icon-set 'unicode)
        (org-ol-tree-ui--update-icon-set)
        (expect org-ol-tree-ui--icon-set
                :to-have-same-items-as
                (plist-get org-ol-tree-ui-icon-set-list 'unicode)))

      (it "it should use the `all-the-icons' set if all-the-icons is available and on GUI"
        (spy-on 'org-ol-tree-system--graphical-frame-p :and-return-value t)
        (spy-on 'org-ol-tree-system--all-the-icons-p :and-return-value t)
        (setq org-ol-tree-ui-icon-set nil)
        (org-ol-tree-ui--update-icon-set)

        (expect org-ol-tree-ui--icon-set
                :to-have-same-items-as
                (plist-get org-ol-tree-ui-icon-set-list 'all-the-icons))
        (expect 'org-ol-tree-system--graphical-frame-p :to-have-been-called-times 1)
        (expect 'org-ol-tree-system--all-the-icons-p :to-have-been-called-times 1))

      (it "it should use the `unicode' set if on GUI but without all-the-icons installed"
        (spy-on 'org-ol-tree-system--graphical-frame-p :and-return-value t)
        (spy-on 'org-ol-tree-system--all-the-icons-p :and-return-value nil)
        (setq org-ol-tree-ui-icon-set nil)
        (org-ol-tree-ui--update-icon-set)

        (expect org-ol-tree-ui--icon-set
                :to-have-same-items-as
                (plist-get org-ol-tree-ui-icon-set-list 'unicode))
        (expect 'org-ol-tree-system--graphical-frame-p :to-have-been-called-times 2)
        (expect 'org-ol-tree-system--all-the-icons-p :to-have-been-called-times 1))

      (it "it should use the `ascii' set if not on GUI"
        (spy-on 'org-ol-tree-system--graphical-frame-p :and-return-value nil)
        (spy-on 'org-ol-tree-system--all-the-icons-p :and-return-value t)
        (setq org-ol-tree-ui-icon-set nil)
        (org-ol-tree-ui--update-icon-set)

        (expect org-ol-tree-ui--icon-set
                :to-have-same-items-as
                (plist-get org-ol-tree-ui-icon-set-list 'ascii))
        (expect 'org-ol-tree-system--graphical-frame-p :to-have-been-called-times 2)
        (expect 'org-ol-tree-system--all-the-icons-p :to-have-been-called-times 0))))

  (describe "Given a document icon,"
      :var (org-ol-tree-ui-icon-set)

    (describe "when it is nil in the icon set,"
      (it "it should have a single white space representing the cursor column"
        (spy-on 'plist-get)
        (expect (org-ol-tree-ui--doc-icon) :to-equal " ")
        (expect 'plist-get :to-have-been-called-with org-ol-tree-ui--icon-set :root)))

    (describe "when the icon set has an empty icon for it,"
      (it "it should have a single white space representing the cursor column"
        (setq org-ol-tree-ui-icon-set 'iconless)
        (org-ol-tree-ui--update-icon-set)

        (expect (org-ol-tree-ui--doc-icon) :to-equal " ")))

    (describe "when the icon set has an actual icon for it,"
      :var (icon)

      (it "it should have a white space in the beginning and one in the end of the icon string"
        (setq org-ol-tree-ui-icon-set 'ascii)
        (org-ol-tree-ui--update-icon-set)
        (setq icon (org-ol-tree-ui--doc-icon))

        (expect icon :to-have-length-greater-than 2)
        (expect icon :to-start-with " ")
        (expect icon :to-end-with " "))))

  (describe "Given a section icon,"
    :var (org-ol-tree-ui-icon-set
          icon)

    (describe "when the section used to get the icon has subsections under it,"
      :var ((heading (org-ol-tree-core--heading-create-internal
                      :name "Test Heading"
                      :id "1.2.3"
                      :marker (point-marker)
                      :level 3
                      :parent nil
                      :subheadings '(sub-heading1 sub-heading2))))

      (before-each
        (setq org-ol-tree-ui-icon-set 'ascii)
        (org-ol-tree-ui--update-icon-set))

      (describe "and the section state is 'expanded,"
        (before-each
          (setq icon (org-ol-tree-ui--section-icon heading 'expanded)))

        (it "it should display the cursor column plus the expanded icon on its beginning"
          (expect icon :to-start-with " - ")))

      (describe "and the section state is 'collapsed,"
        (before-each
          (setq icon (org-ol-tree-ui--section-icon heading 'collapsed)))

        (it "it should display the cursor column plus the collapsed icon on its beginning"
          (expect icon :to-start-with " + "))))

    (describe "when the section used to get the icon does not have subsections under it,"
      :var ((heading (org-ol-tree-core--heading-create-internal
                      :name "Test Heading"
                      :id "1.2.3"
                      :marker (point-marker)
                      :level 3
                      :parent nil
                      :subheadings nil)))

      (before-each
        (setq org-ol-tree-ui-icon-set 'ascii)
        (org-ol-tree-ui--update-icon-set)
        (setq icon (org-ol-tree-ui--section-icon heading nil)))

      (it "it should start with a cursor column space plus two spaces for alignment"
        (expect icon :to-start-with "   ")))

    (describe "when the section icon contains the %(section) tag"
      :var ((heading (org-ol-tree-core--heading-create-internal
                      :name "Test Heading"
                      :id "1.2.3"
                      :marker (point-marker)
                      :level 3
                      :parent nil
                      :subheadings nil)))

      (before-each
        (setq org-ol-tree-ui-icon-set 'ascii
              icon (org-ol-tree-ui--section-icon heading nil))
        (org-ol-tree-ui--update-icon-set))

      (it "it should show the section id on the icon"
        (expect icon :to-match "1\\.2\\.3")))

    (describe "when the section icon does not contain the %(section) tag"
      :var ((heading (org-ol-tree-core--heading-create-internal
                      :name "Test Heading"
                      :id "1.2.3"
                      :marker (point-marker)
                      :level 3
                      :parent nil
                      :subheadings nil)))

      (before-each
        (setq org-ol-tree-ui-icon-set 'iconless-ascii)
        (org-ol-tree-ui--update-icon-set)
        (setq icon (org-ol-tree-ui--section-icon heading nil)))

      (it "it should show the section id on the icon"
        (expect icon :not :to-match "1\\.2\\.3")))))



(describe "Window"
  )



(describe "Buffer"
  )



(provide 'test-outline-ui)
;;; test-outline-ui.el ends here
