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
(load-file "./tests/buttercup-matchers.el")

(require 'org-ol-tree)
(require 'buttercup)
(require 's)
(require 'seq)


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



(describe "Window:"

  (before-each
    (setq-default split-width-threshold 0
                  split-height-threshold 0
                  window-min-width 0
                  window-min-height 0))


  (describe "Given an Org file,"
    :var (org-buffer current-window ol-window header)

    (before-each
      (find-file (expand-file-name "data/doc-happy-path.org" test-root-dir))
      (setq org-buffer (current-buffer)))

    (it "it should not have any Outline window associated with it"
      (expect (org-ol-tree-ui--visibility) :to-be 'none))


    (describe "when requesting to open the Outline window,"

      (before-each
        (call-interactively 'org-ol-tree))

      (it "it should focus on the Outline window"
        (expect org-ol-tree--buffer-p :to-be-truthy))

      (it "the cursor should be located on the root header"
        (setq header ()))


      (describe "and checking the Outline visibility,"

        (before-each
          (select-window (get-buffer-window org-ol-tree--org-buffer)))

        (it "it should be 'visible"
          (expect (org-ol-tree-ui--visibility) :to-be 'visible)))


      (describe "and closing the outline window,"

        (before-each
          (delete-window))

        (it "the focus should move back to Org file"
          (expect org-ol-tree--buffer-p :not :to-be-truthy))

        (it "the Outline visibility should be 'exist"
          (expect (org-ol-tree-ui--visibility) :to-be 'exists)))
      )



    ))



(describe "Buffer"
  )



(provide 'test-outline-ui)
;;; test-outline-ui.el ends here
