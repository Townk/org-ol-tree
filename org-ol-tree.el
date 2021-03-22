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
;; Package-Requires: ((emacs "27.1") (org "9.5") (all-the-icons "4.0.1") (treemacs "2.8") (s "1.12.0") (seq))
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
(require 's)
(require 'all-the-icons)
(require 'seq)
(require 'subr-x)


(defvar-local org-ol-tree--target-buffer nil
  "A buffer local variable used to hold the buffer object where the outline
should act on.")


;;;; Helper functions

;;; Sections

(defun org-ol-tree--section-to-string (section-stack)
  "Convert a list of numbers into a section number string notation.

This function does the conversion by transforming each element from
SECTION-STACK into a string, reversing the list, and joining all elements with
a '.' character.

Example::
  (org-ol-tree--section-to-string '(3 2 1))

  ;; => \"1.2.3\""
  (when (and section-stack
             (seq-every-p 'number-or-marker-p section-stack))
    (string-join (reverse (mapcar 'number-to-string section-stack))
                 ".")))


(defun org-ol-tree--section-to-stack (section-string)
  "Convert SECTION-STRING into a section stack.

A section stack is simply a list with all the numbers from a section as
individual numbers on the reverse order do they appear.

Example::
  (org-ol-tree--section-to-stack \"1.2.3\")

  ;; => (3 2 1)"
  (when (and section-string
             (stringp section-string)
             (string-match-p "^[0-9]+\\(\\.[0-9]+\\)*$" section-string))
    (reverse (mapcar 'string-to-number (split-string section-string "\\.")))))


(defun org-ol-tree--next-section (section-stack target-level)
  "Return a new SECTION-STACK for the next section on TARGET-LEVEL.

For more information on the meaning of a section-stack, look the
`org-ol-tree--section-to-stack' documentation.

Examples::
  (org-ol-tree--next-section '(3 2 1) 3)  ;; => (4 2 1)
  (org-ol-tree--next-section '(3 2 1) 2)  ;; => (3 1)
  (org-ol-tree--next-section '(3 2 1) 4)  ;; => (1 3 2 1)"
  (let ((curr-level (length section-stack))
        (new-stack section-stack))
    (cond ((> curr-level target-level)
           (setq new-stack (seq-drop section-stack (- curr-level target-level))
                 new-stack (cons (1+ (car new-stack)) (cdr new-stack))))
          ((< curr-level target-level)
           (push 1 new-stack))
          (t
           (setcar new-stack (1+ (car new-stack)))))
    new-stack))


;;; Heading object

(defun org-ol-tree--get-heading-info (section-stack)
  "Return a property list representing the heading from `point'.

The SECTION-STACK is a list of numbers representing the current section number
\(see `org-ol-tree--section-to-stack' function documentation for details on a
section stack.)."
  (let* ((this-components (org-heading-components))
         (this-level (nth 0 this-components)))
    `(:ol-name ,(nth 4 this-components)
      :ol-section ,(org-ol-tree--section-to-string (org-ol-tree--next-section section-stack this-level))
      :ol-buffer ,(current-buffer)
      :ol-point ,(point)
      :ol-level ,this-level
      :ol-todo ,(nth 2 this-components)
      :ol-priority ,(nth 3 this-components)
      :ol-tags ,(org-get-tags nil t)
      :ol-children ,(list))))


;;; Document stack

(defun org-ol-tree--merged-stack (stack to-level)
  "Reduce the headings STACK to the giving TO-LEVEL.

The STACK is a list of heading lists. Each heading list represents all headings
\(a.k.a. sections) for a giving level. The entire `cdr' of this list should
already be resolved with its children already added, the only heading still
being evaluated should be the `car' of the list.

This function gets the `car' of the sta This function adds each merged `car'
into the property `:ol-children' of the next element."
  (let ((new-stack stack)
        (curr-level (plist-get (car (car stack)) :ol-level)))
    (while (> curr-level to-level)
      (let ((head (reverse (car new-stack))))
        (setq new-stack (cdr new-stack))
        (plist-put (car (car new-stack)) :ol-children head)
        (setq curr-level (plist-get (car (car new-stack)) :ol-level))))
    new-stack))


(defun org-ol-tree--query-doc ()
  "In an Org file, traverse it to create a tree-like structure for headings.

This function uses the `outline-next-heading' function to traverse the org file
and uses the structure-like list returned by `org-ol-tree--get-heading-info' as
node information."
  (when (bound-and-true-p org-ol-tree--target-buffer)
    (with-current-buffer org-ol-tree--target-buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((curr-level 0)
                (doc (list))
                (section-stack (list)))
            (while (outline-next-heading)
              (let* ((this-heading (org-ol-tree--get-heading-info section-stack))
                     (this-level (plist-get this-heading :ol-level)))
                (cond ((= curr-level this-level) (push this-heading (car doc)))
                      ((< curr-level this-level) (push `(,this-heading) doc))
                      (t (progn
                           (setq doc (org-ol-tree--merged-stack doc this-level))
                           (push this-heading (car doc)))))
                (setq curr-level this-level
                      section-stack (org-ol-tree--section-to-stack
                                     (plist-get this-heading :ol-section)))))
            (reverse (car (org-ol-tree--merged-stack doc 1)))))))))


;;;; UI functions

;;; Icons

(defun org-ol-tree--node-icon (&optional section-id has-children-p open-p)
  "Return a propertized string representing an icon on org-ol-tree.

When not nil, SECTION-ID should be a string representing a section (e.g.
\"2.1.3\"). A nil value represents the root node.

HAS-CHILDREN-P is a boolean indicating if the requested node has children nodes.

If OPEN-P is a non nil value, it indicates the requested node state is open. If
HAS-CHILDREN-P is nil, this argument is ignored."
  (if section-id
      (concat
       (if has-children-p
           (propertize
            "--"
            'face 'doom-themes-treemacs-file-face
            'display (all-the-icons-material (if open-p "keyboard_arrow_down" "chevron_right")
                                             :height 0.95
                                             :v-adjust -0.17))
         "  ")
       (propertize (format "§ %s - " section-id) 'face 'doom-themes-treemacs-file-face))
    (concat
     " "
     (propertize "--"
                 'face 'treemacs-root-face
                 'display (all-the-icons-material "description" :height 0.95 :v-adjust -0.17))
     " ")))


(defun org-ol-tree--get-root-label ()
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
         ((re-search-forward "^\\+TITLE:[ \t]*\\([^\n]+\\)" nil t)
          (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
         ((buffer-file-name)
          (s-titleized-words (buffer-file-name)))
         (t (s-titleized-words (buffer-name))))))))


;;;; Action functions

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

  (unless
      (when-let ((buffer (and (bound-and-true-p org-ol-tree--target-buffer)
                              (buffer-live-p org-ol-tree--target-buffer)
                              org-ol-tree--target-buffer))
                 (node (treemacs-current-button))
                 (window (get-buffer-window))
                 (heading-loc (treemacs-button-get node :ol-point)))
        (select-window (next-window))
        (switch-to-buffer buffer)
        (let ((narrow-p (or (and (not narrow-p) (buffer-narrowed-p))
                            (and narrow-p (not (buffer-narrowed-p))))))
          (widen)
          (goto-char heading-loc)
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


;;;; Treemacs extension

(defmacro org-ol-tree--render-action ()
  "Macro that produces the strings required to render ITEM as a treemacs node.

This macro is a thin wrapper around `treemacs-render-node' that sets the
correct values according to the given section ITEM.

The ITEM structure is defined on the `:more-properties' value given to the
`treemacs-render-node'."
  (let ((heading-location '(plist-get item :ol-point))
        (section-id '(plist-get item :ol-section))
        (section-name '(plist-get item :ol-name))
        (subsections '(plist-get item :ol-children)))
    `(treemacs-render-node :icon (org-ol-tree--node-icon ,section-id ,subsections)
                          :label-form ,section-name
                          :state (if ,subsections
                                     treemacs-org-ol-parent-section-closed-state
                                   treemacs-org-ol-section-state)
                          :key-form ,section-id
                          :face 'treemacs-file-face
                          :more-properties (:ol-children ,subsections
                                            :ol-section ,section-id
                                            :ol-name ,section-name
                                            :ol-point ,heading-location))))


(treemacs-define-leaf-node org-ol-section 'dynamic-icon
                           :ret-action #'org-ol-tree/visit-section
                           :mouse1-action #'org-ol-tree/visit-section)


(treemacs-define-expandable-node org-ol-parent-section
  :icon-open-form (org-ol-tree--node-icon (treemacs-button-get node :ol-section) t t)
  :icon-closed-form (org-ol-tree--node-icon (treemacs-button-get node :ol-section) t nil)
  :ret-action #'org-ol-tree/visit-section
  :query-function (treemacs-button-get node :ol-children)
  :render-action (org-ol-tree--render-action))


(treemacs-define-expandable-node org-ol-doc
  :icon-open (org-ol-tree--node-icon)
  :icon-closed (org-ol-tree--node-icon)
  :query-function (org-ol-tree--query-doc)
  :render-action (org-ol-tree--render-action)
  :top-level-marker t
  :root-face 'treemacs-root-face
  :root-key-form 'Outline
  :root-label (org-ol-tree--get-root-label))


;;;; Commands

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
