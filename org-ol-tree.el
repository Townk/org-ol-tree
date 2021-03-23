;;; org-ol-tree.el --- an outline window for Org files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thiago Alves
;;
;; Author: Thiago Alves <thiago@rapinialves.com>
;; Maintainer: Thiago Alves <thiago@rapinialves.com>
;; Created: March 20, 2021
;; Version: 0.0.1
;; Keywords: org, org-mode, outline, tree, tree-view, treeview, treemacs
;; Homepage: https://github.com/Townk/org-ol-tree
;; Package-Requires: ((emacs "27.1") (org "9.5") (all-the-icons "4.0.1") (treemacs "2.8") (dash "2.18.1") (s "1.12.0") (seq) (cl-lib))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; TODO: Add a good description for the package that is not a copy
;;       of the README.org file.
;;
;;; Code:

(require 'org)
(require 'treemacs)
(require 'dash)
(require 's)
(require 'all-the-icons)
(require 'seq)
(require 'subr-x)


(defvar-local org-ol-tree--target-buffer nil
  "A buffer local variable used to hold the buffer object where the outline
should act on.")



;;;; ---- Helper functions

;;; Sections

(defun org-ol-tree-section-p (stack-or-string)
  "Return t when STACK-OR-STRING is a valid section object.

If STACK-OR-STRING is a list, all its elements should be numbers representing
individual numbers from a section id as on the reverse order do they appear.
For instance, the section \"1.3.2 My section text\" would be represented as
\(2 3 1) on the stack form.

If STACK-OR-STRING is a string it should have only integers separated by
dots as they would appear on a setion heading. For instance, the same
\"1.3.2 My section text\" heading is represented by the string \"1.3.2\"."
  (and stack-or-string
       (or (and (stringp stack-or-string)
                (string-match-p "^[0-9]+\\(\\.[0-9]+\\)*$" stack-or-string))
           (and (listp stack-or-string)
                (seq-every-p 'number-or-marker-p stack-or-string)))))


(defun org-ol-tree-section-stack-to-string (section-stack)
  "Convert a list of numbers into a section number string notation.

This function does the conversion by transforming each element from
SECTION-STACK into a string, reversing the list, and joining all elements with
a '.' character.

Example::
  (org-ol-tree-section-stack-to-string '(3 2 1))

  ;; => \"1.2.3\""
  (when (org-ol-tree-section-p section-stack)
    (string-join (reverse (mapcar 'number-to-string section-stack)) ".")))


(defun org-ol-tree-section-string-to-stack (section-string)
  "Convert SECTION-STRING into a section stack.

Example::
  (org-ol-tree-section-string-to-stack \"1.2.3\")

  ;; => (3 2 1)"
  (when (org-ol-tree-section-p section-string)
    (reverse (mapcar 'string-to-number (split-string section-string "\\.")))))


(defun org-ol-tree-section-next (section target-level)
  "Return a new section-stack for the next SECTION on TARGET-LEVEL.

For more information on the meaning of a section-stack, look the
`org-ol-tree-section-p' documentation.

Examples::
  (org-ol-tree-section-next '(3 2 1) 3)  ;; => (4 2 1)
  (org-ol-tree-section-next '(3 2 1) 2)  ;; => (3 1)
  (org-ol-tree-section-next '(3 2 1) 4)  ;; => (1 3 2 1)"

  (if (or (not section)
          (and (stringp section) (string-empty-p section)))
      (-repeat target-level 1)

    (unless (org-ol-tree-section-p section)
      (error "The given section object is not an org-ol-tree-section"))

    (let* ((section (if (stringp section) (org-ol-tree-section-string-to-stack section) section))
           (target-level (max 1 target-level))
           (curr-level (length section))
           (new-stack section))
      (cond ((> curr-level target-level)
             (setq new-stack (seq-drop section (- curr-level target-level))
                   new-stack (cons (1+ (car new-stack)) (cdr new-stack))))
            ((< curr-level target-level)
             (setq new-stack (append (-repeat (- target-level curr-level) 1) new-stack)))
            (t
             (setcar new-stack (1+ (car new-stack)))))
      new-stack)))


;;; Heading object

(cl-defstruct (org-ol-tree-heading (:constructor org-ol-tree-heading--create)
                                    (:copier nil))
  "The Org Outline Tree heading structure.

It has the basic information to build and draw a tree-like structure
representing an entire org document."
  (name :type string
        :documentation "The org heading text with no decorations.")
  (id :type string
      :documentation "A string representing the section number.")
  (marker :type marker
          :documentation "Location of this heading on its org file buffer.")
  (level :type number
         :documentation "The nested level for this org heading.")
  (parent :type org-ol-tree--heading
          :documentation "The parent heading or nil if this is a root heading.")
  (subheadings (list)
               :type list
               :documentation "A collection of children headings."))

(cl-defun org-ol-tree-heading-create (&optional previous-heading)
  "Create a new `org-ol-tree-heading' from `point' on current org buffer.

If PREVIOUS-HEADING is non nil, this function creates the new heading as a
subheading of the given parent.

If `current-buffer' is not an org buffer, or `point' is not over an org heading,
this functions raises a user error."
  (unless (and (eq major-mode 'org-mode)
               (org-at-heading-p))
    (user-error "Cannot create an org-ol-tree-heading with cursor outside an actual org headline"))

  (unless (or (null previous-heading)
              (org-ol-tree-heading-p previous-heading))
    (error "Given parent must be nil or a 'org-ol-tree-heading' object"))

  (message "Previous heading level: %s" (if (null previous-heading)
                                            0
                                          (org-ol-tree-heading-level previous-heading)))

  (let* ((previous-level (if (null previous-heading)
                           0
                         (org-ol-tree-heading-level previous-heading)))
         (previous-id (when (org-ol-tree-heading-p previous-heading)
                        (org-ol-tree-heading-id previous-heading)))
         (this-components (org-heading-components))
         (this-name (nth 4 this-components))
         (this-level (nth 0 this-components))
         (this-marker (point-marker))
         (this-parent (cond
                       ((= this-level previous-level)
                        (org-ol-tree-heading-parent previous-heading))
                       ((> this-level previous-level) previous-heading)
                       ((< this-level previous-level)
                        (let ((node previous-heading)
                              (node-level previous-level))
                          (while (and (not (null node))
                                      (<= this-level node-level))
                            (setq node (org-ol-tree-heading-parent node))
                            (if (null node)
                                (setq node-level 0)
                              (setq node-level (org-ol-tree-heading-level node))))
                          node))
                       (t nil)))
         (this-section-stack (org-ol-tree-section-next previous-id this-level))
         (this-section-id (org-ol-tree-section-stack-to-string this-section-stack))
         (this-heading (org-ol-tree-heading--create :name this-name
                                                    :id this-section-id
                                                    :marker this-marker
                                                    :level this-level
                                                    :parent this-parent)))
    (when this-parent
      (push this-heading (org-ol-tree-heading-subheadings this-parent)))
    this-heading))


(defun org-ol-tree-traverse-doc ()
  "In an Org file, traverse it to create a tree-like structure for headings.

This function uses the `outline-next-heading' function to traverse the org file
and uses the cl-struct `org-ol-tree-heading' as node information."
  (when (bound-and-true-p org-ol-tree--target-buffer)
    (with-current-buffer org-ol-tree--target-buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((doc (list))
                current-heading)
            (while (outline-next-heading)
              (let ((this-heading (org-ol-tree-heading-create current-heading)))
                (unless (org-ol-tree-heading-parent this-heading)
                  (push this-heading doc))
                (setq current-heading this-heading)))
            doc))))))



;;;; ---- UI functions

;;; Icons

(defun org-ol-tree-root-button-icon ()
  "Return the string used as the icon for the root element."
  (concat
   " "
   (propertize "--"
               'face 'treemacs-root-face
               'display (all-the-icons-material "description" :height 0.95 :v-adjust -0.17))
   " "))


(defun org-ol-tree-root-button-label ()
  "Return a string label for the outline root node.

The label is given by the title on the target buffer if one is defined, by the
file name of the target buffer transformed to title case, if the target buffer
has a file associated with it, or by the target's buffer name transformed to
title case."
  (when (bound-and-true-p org-ol-tree--target-buffer)
    (with-current-buffer org-ol-tree--target-buffer
      (save-excursion
        (goto-char (point-min))
        (cond
         ((re-search-forward "^#\\+TITLE:[ \t]*\\([^\n]+\\)" nil t)
          (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
         ((buffer-file-name)
          (s-titleized-words (file-name-base (buffer-file-name))))
         (t (s-titleized-words (buffer-name))))))))


(defun org-ol-tree-button-icon (heading state)
  "Return the full icon for the giving HEADING.

The icon depends on the icon theme configuration as well as the expandable
state of HEADING.

The STATE argument indicates if this icon should represent an open or closed
node.Valid values for STATE are 'expanded,'collapsed, and nil. In practice,
this function considers the state as 'collapsed for any value non nil and
different than 'expanded."
  (concat " "
          (if (org-ol-tree-heading-subheadings heading)
              (propertize "--"
                          'face 'doom-themes-treemacs-file-face
                          'display (all-the-icons-material (if (eq state 'expanded)
                                                               "keyboard_arrow_down"
                                                             "chevron_right")
                                                           :height 0.95
                                                           :v-adjust -0.17))
            "  ")
          (propertize (format "§ %s " (org-ol-tree-heading-id heading))
                      'face
                      'doom-themes-treemacs-file-face)))



;;;; ---- Action functions

;;; Visit node

(defun org-ol-tree/visit-section (&optional narrow-p &rest _)
  "Switch to the buffer saved in node at point.

When this function is invoked with a prefix argument, NARROW-P is set to a
non-nil value and it toggles the narrowed state. For instance, if your buffer
is not narrowed, invoking this function with a prefix argument causes the
selected section to get narrowed. From now on, subsequent calls of this feature
narrow the selected section, until you call it with th universal argument
again, which causes the buffer to get widen."
  (interactive "P")

  (if-let ((buffer (and (bound-and-true-p org-ol-tree--target-buffer)
                        (buffer-live-p org-ol-tree--target-buffer)
                        org-ol-tree--target-buffer))
           (node (treemacs-current-button))
           (window (get-buffer-window))
           (doc-window (or (get-buffer-window org-ol-tree--target-buffer)
                           (next-window)))
           (heading (treemacs-button-get node :heading))
           (heading-marker (org-ol-tree-heading-marker heading)))
      (progn
        (select-window doc-window)
        (switch-to-buffer buffer)
        (let ((narrow-p (or (and (not narrow-p) (buffer-narrowed-p))
                            (and narrow-p (not (buffer-narrowed-p))))))
          (widen)
          (goto-char heading-marker)
          (org-reveal)
          (org-show-entry)
          ;; (evil-scroll-line-to-top nil)
          (recenter (min (max 0 scroll-margin)
                         (truncate (/ (window-body-height) 4.0)))
                    t)
          (when narrow-p
            (org-narrow-to-subtree))
          (select-window window)
          (treemacs-pulse-on-success)
          t))
    (user-error "No section information found on current point")))



;;;; ---- Treemacs extension

(treemacs-define-leaf-node org-ol-section 'dynamic-icon
                           :ret-action #'org-ol-tree/visit-section
                           :mouse1-action #'org-ol-tree/visit-section)


(treemacs-define-expandable-node org-ol-parent-section
  :icon-open-form (org-ol-tree-button-icon (treemacs-button-get node :heading) 'expanded)
  :icon-closed-form (org-ol-tree-button-icon (treemacs-button-get node :heading) 'collapsed)
  :ret-action 'org-ol-tree/visit-section
  :query-function (reverse (org-ol-tree-heading-subheadings (treemacs-button-get node :heading)))
  :render-action
  (treemacs-render-node :icon (org-ol-tree-button-icon item 'collapsed)
                        :label-form (org-ol-tree-heading-name item)
                        :state (if (org-ol-tree-heading-subheadings item)
                                   treemacs-org-ol-parent-section-closed-state
                                 treemacs-org-ol-section-state)
                        :key-form (org-ol-tree-heading-id item)
                        :face 'treemacs-file-face
                        :more-properties (:heading item)))


(treemacs-define-expandable-node org-ol-doc
  :icon-open (org-ol-tree-root-button-icon)
  :icon-closed (org-ol-tree-root-button-icon)
  :query-function (reverse (org-ol-tree-traverse-doc))
  :top-level-marker t
  :root-face 'treemacs-root-face
  :root-key-form 'Outline
  :root-label (org-ol-tree-root-button-label)
  :render-action
  (treemacs-render-node :icon (org-ol-tree-button-icon item 'collapsed)
                        :label-form (org-ol-tree-heading-name item)
                        :state treemacs-org-ol-parent-section-closed-state
                        :key-form (org-ol-tree-heading-id item)
                        :face 'treemacs-file-face
                        :more-properties (:heading item)))



;;;; ---- Commands

;;;###autoload
(defun org-ol-tree/display-sections ()
  "Open a side window with the Org file outlined.

This function is the starting point for the Org Outline Tree package. It will
create and setup a side window, and invoke the treemacs extension to populate
the outline."
  (interactive)

  (unless (eq major-mode 'org-mode)
    (error "Can't use Org Outline Tree on non-Org buffers"))

  (let* ((origin-buffer (current-buffer))
         (buffer (get-buffer-create (format "*OrgOutlineTree:%s*" (buffer-name origin-buffer))))
         (window (display-buffer-in-side-window buffer '((side . right)))))
    ;; (set-window-margins window 2 2)
    (select-window window)
    (treemacs-initialize)
    (setq-local org-ol-tree--target-buffer origin-buffer
                treemacs--width-is-locked nil
                window-size-fixed nil
                treemacs-mode-map nil)
    (setq header-line-format '("☰ Outline"))
    (set-window-buffer nil (current-buffer))
    (treemacs-ORG-OL-DOC-extension)
    (treemacs-expand-org-ol-doc)
    (beginning-of-line)))


(provide 'org-ol-tree)
;;; org-ol-tree.el ends here
