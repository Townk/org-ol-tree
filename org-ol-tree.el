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
;; Package-Requires: ((emacs "27.1") (org "9.5") (treemacs "2.8") (dash "2.18.1") (s "1.12.0") (seq) (cl-lib))
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
(require 'seq)
(require 'subr-x)



;;;; ---- Variables

;;; Constants

(defconst org-ol-tree-packages--all-the-icons-p (require 'all-the-icons nil 'noerror)
  "Constant indicating if package all-the-icons is installed.")

(defconst org-ol-tree-packages--evil-p (require 'evil-core nil 'noerror)
  "Constant indicating if package evil is installed.")

;;; Private variables

(defvar-local org-ol-tree--org-buffer nil
  "A buffer local variable used to hold the buffer object where the outline
should act on.")

(defvar-local org-ol-tree-heading--root nil
  "Hold the root node for the displayed outline.")

(defvar-local org-ol-tree-heading--root-dirt-p nil
  "Flag indicating the root hading needs to be rebuild.")

(defvar-local org-ol-tree-input--debounce-timer nil
  "The timer waiting to debounce click operations on the tree view.")

(defvar org-ol-tree-icons--selected-theme nil
  "This variable holds the current icon theme used by the outline.

To know how this variable is populated, check the `org-ol-tree-icons-theme'
documentation.

Never update this variable manually. It is intended to self-mutate when calling
the `org-ol-tree-icons-update-theme' function.")


;;; Configuration variables

(defvar org-ol-tree-mode-map (-doto (make-sparse-keymap)
                               (define-key [mouse-1]  #'org-ol-tree-input--leftclick-action)
                               (define-key [double-mouse-1] #'org-ol-tree-input--doubleclick-action)
                               (define-key (kbd "<left>") #'org-ol-tree-navigation--collapse-current)
                               (define-key (kbd "<right>") #'org-ol-tree-navigation--expand-current))
  "Key bindings for the `org-ol-tree-mode'.")


(when org-ol-tree-packages--evil-p
  (evil-define-key '(normal) org-ol-tree-mode-map
    "h" #'org-ol-tree-navigation--collapse-current
    "l" #'org-ol-tree-navigation--expand-current
    (kbd "<left>") #'org-ol-tree-navigation--collapse-current
    (kbd "<right>") #'org-ol-tree-navigation--expand-current
    ))


(defvar org-ol-tree-window-position 'right
  "Symbol indicating where to open the outline window.

Usually, the value of this variable is `left' or `right'.")


(defvar org-ol-tree-icons-theme-plist
  (-non-nil
   (append '()
          (when org-ol-tree-packages--all-the-icons-p
            (list 'all-the-icons `(:root ,(all-the-icons-material "description"
                                                                  :height 0.95
                                                                  :v-adjust -0.17)
                                   :expanded ,(all-the-icons-material "keyboard_arrow_down"
                                                                      :height 0.95
                                                                      :v-adjust -0.17)
                                   :collapsed ,(all-the-icons-material "chevron_right"
                                                                       :height 0.95
                                                                       :v-adjust -0.17)
                                   :section "§ %(section)")))
          (list 'unicode `(:root "■"  ; <-- this is bad! Accepting suggestions for a better one!
                           :expanded "▾ "
                           :collapsed "▸ "
                           :section "§ %(section)")
                'ascii `(:root "*"
                         :expanded "- "
                         :collapsed "+ "
                         :section "%(section)"))
          (when org-ol-tree-packages--all-the-icons-p
            (list 'iconless-fancy `(:root ""
                                    :expanded ,(all-the-icons-material "keyboard_arrow_down"
                                                                       :height 0.95
                                                                       :v-adjust -0.17)
                                    :collapsed ,(all-the-icons-material "chevron_right"
                                                                        :height 0.95
                                                                        :v-adjust -0.17)
                                    :section "")))
          (list 'iconless-unicode `(:root ""
                                    :expanded "▾ "
                                    :collapsed "▸ "
                                    :section "")
                'iconless-ascii `(:root ""
                                  :expanded "- "
                                  :collapsed "+ "
                                  :section "")
                'iconless `(:root ""
                            :expanded ""
                            :collapsed ""
                            :section ""))))
  "A property list of property list representing all icon themes available.

An icon theme is a simple property list with four entries:

  `:root' - The root node icon for the outline, displayed on the left of the
            document's title;
  `:expanded' - The icon displayed on the left of expandable nodes that are
                currently expanded;
  `:collapsed' - The icon displayed on the left of expandable nodes that are
                 currently collapsed;
  `:sectiion' - The icon displayed on the left side of all section heading on
                the outline;

All icons are strings. The `:root', `:xpanded', and `collapsed' icons are
displayed as the `display' property of a two-characters propertized string.
This is done to guarantee the proper alignment of icons when using unicode or
all-the-icons icons.

The `:section' icon is actually a simple one-line string where we replace the
string \"%(section)\" by the section number of the heading. Different then the
other icons, the `:section' icon allows any arbitrary size string.")


(defvar org-ol-tree-icons-theme nil
  "The theme to use on the outline icons.

To check all available themes, look into `org-ol-tree-icons-theme-plist'
documentation.

The outline chooses the theme based on the following criteria:

1) If this variable is not nil, and the symbol from it is one of the available
   themes, the theme indicated by the variable is used;
2) If the Emacs frame is a graphical frame, and the package all-the-icons is
   installed and available, the theme `all-the-icons' is used;
3) If the Emacs frame is a graphical frame, and the package all-the-icons is
   NOT installed nor available, the theme `unicode' is used;
4) Fallback to the `ascii' theme;")



;;;; ---- Helper functions

;;; Sections

(defun org-ol-tree-section-p (stack-or-string)
  "Return t when STACK-OR-STRING is a valid section object.

If STACK-OR-STRING is a list, all its elements should be numbers representing
individual numbers from a section id as on the reverse order do they appear.
For instance, the section \"1.3.2 My section text\" would be represented as
\(2 3 1) on the stack form.

If STACK-OR-STRING is a string it should have only integers separated by
dots as they would appear on a section heading. For instance, the same
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


;;; Headings

(cl-defstruct (org-ol-tree-heading (:constructor org-ol-tree-heading--create)
                                   (:copier nil))
  "The Org Outline Tree heading structure.

It has the basic information to build and draw a tree-like structure
representing an entire org document."
  (name nil
        :type string
        :documentation "The org heading text with no decorations.")
  (id nil
      :type string
      :documentation "A string representing the section number.")
  (marker nil
          :type marker
          :documentation "Location of this heading on its org file buffer.")
  (level nil
         :type number
         :documentation "The nested level for this org heading.")
  (parent nil
          :type org-ol-tree--heading
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


(defun org-ol-tree-heading-current ()
  "Return the heading object for the tree node under the cursor.

If cursor is outside a heading node, return nil."
  (treemacs-with-current-button
   "No heading node found under the cursor"
   (or (treemacs-button-get current-btn :heading)
       (when (= (line-number-at-pos) 1) (org-ol-tree-heading-root)))))


(defun org-ol-tree-heading-root-label ()
  "Return a string label for the outline root node.

The label is given by the title on the target buffer if one is defined, by the
file name of the target buffer transformed to title case, if the target buffer
has a file associated with it, or by the target's buffer name transformed to
title case."
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward "^#\\+TITLE:[ \t]*\\([^\n]+\\)" nil t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
     ((buffer-file-name)
      (s-titleized-words (file-name-base (buffer-file-name))))
     (t (s-titleized-words (buffer-name))))))


(defun org-ol-tree-heading-root-build (&optional buffer-or-name)
  "Traverse BUFFER-OR-NAME buffer to create a tree-like structure for headings.

This function uses the `outline-next-heading' function to traverse the org file
and uses the cl-struct `org-ol-tree-heading' as node information.

If BUFFER-OR-NAME is nil, uses the `current-buffer'. If the given buffer is not
an Org buffer, raises a `user-error'."
  (let ((buffer (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (user-error "Can't traverse an Org document on a NON-Org buffer"))

      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((root (org-ol-tree-heading--create :name (org-ol-tree-heading-root-label)
                                                   :id 'Outline
                                                   :marker (point-min)
                                                   :level 0))
                current-heading)
            (while (outline-next-heading)
              (let ((this-heading (org-ol-tree-heading-create current-heading)))
                (unless (org-ol-tree-heading-parent this-heading)
                  (push this-heading (org-ol-tree-heading-subheadings root)))
                (setq current-heading this-heading)))
            root))))))


(defun org-ol-tree-heading-root ()
  "Return the root heading for the outline in the current window.

The root heading is cached on a buffer local variable"
  (unless (and (bound-and-true-p org-ol-tree-heading--root) (not org-ol-tree-heading--root-dirt-p))
    (when (bound-and-true-p org-ol-tree--org-buffer)
      (setq-local org-ol-tree-heading--root
                  (org-ol-tree-heading-root-build org-ol-tree--org-buffer))))
  org-ol-tree-heading--root)



;;;; ---- UI functions

;;; Icons

(defun org-ol-tree-icons-update-theme ()
  "Refresh `org-ol-tree-icons--selected-theme' values.

To know how this function populate the `org-ol-tree-icons--selected-theme',
check the `org-ol-tree-icons-theme' variable documentation."
  (setq org-ol-tree-icons--selected-theme
        (plist-get org-ol-tree-icons-theme-plist
                   (cond
                    ((member org-ol-tree-icons-theme org-ol-tree-icons-theme-plist)
                     org-ol-tree-icons-theme)
                    ((and (member window-system '(x w32 ns))
                          org-ol-tree-packages--all-the-icons-p)
                     'all-the-icons)
                    ((member window-system '(x w32 ns))
                     'unicode)
                    (t 'ascii)))))


(defun org-ol-tree-icons-root-icon ()
  "Return the string used as the icon for the root element."
  (let* ((root-icon (plist-get org-ol-tree-icons--selected-theme :root))
         (display-p (> (length root-icon) 0)))
    (concat
     " "
     (when display-p (propertize "--" 'face 'treemacs-root-face 'display root-icon))
     (when display-p " "))))


(defun org-ol-tree-icons-section-icon (heading state)
  "Return the full icon for the giving HEADING.

The icon depends on the icon theme configuration as well as the expandable
state of HEADING.

The STATE argument indicates if this icon should represent an open or closed
node.Valid values for STATE are 'expanded,'collapsed, and nil. In practice,
this function considers the state as 'collapsed for any value non nil and
different than 'expanded."
  (let ((expanded-icon (plist-get org-ol-tree-icons--selected-theme :expanded))
        (collapsed-icon (plist-get org-ol-tree-icons--selected-theme :collapsed))
        (section-icon (plist-get org-ol-tree-icons--selected-theme :section)))
  (concat
   " "
   (if (org-ol-tree-heading-subheadings heading)
       (propertize "--"
                   'face 'doom-themes-treemacs-file-face
                   'display (if (eq state 'expanded) expanded-icon collapsed-icon))
     "  ")
   (when (> (length section-icon) 0)
     (propertize
      (format "%s " (s-replace "%(section)" (org-ol-tree-heading-id heading) section-icon))
      'face
      'doom-themes-treemacs-file-face)))))



;;;; ---- Action functions

;;; Mouse interaction

(defun org-ol-tree-input--leftclick-action (event)
  "Function used to perform a mouse click on a node.

The action triggered by this function depends if the node is a leaf or an
expandable node. If leaf, it will trigger the `treemacs-RET-action', and if it
is an expandable node, it will trigger the `treemacs-TAB-action'. A potential
prefix EVENT is passed on to the executed action, if possible."
  (interactive "e")

  (when org-ol-tree-input--debounce-timer
    (cancel-timer org-ol-tree-input--debounce-timer))

  (setq org-ol-tree-input--debounce-timer
        (run-with-idle-timer 0.1 nil #'org-ol-tree-input--expand-or-visit-current-node event)))

(defun org-ol-tree-input--expand-or-visit-current-node (event)
  "Helper function called by `org-ol-tree-input--leftclick-action'.

This function performs the actual action for the click operation.

The argument EVENT, is the same event received by the
`org-ol-tree-input--leftclick-action' function."
  (setq org-ol-tree-input--debounce-timer nil)

  (when (eq 'mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))

    (when (region-active-p)
      (keyboard-quit))

    (when-let ((heading (org-ol-tree-heading-current)))
      (treemacs-with-current-button
       "No heading node found under the cursor"
       (if (org-ol-tree-heading-subheadings heading)
           (pcase (treemacs-button-get current-btn :state)
             ('treemacs-org-ol-doc-open-state (treemacs-collapse-org-ol-doc))
             ('treemacs-org-ol-doc-closed-state (treemacs-expand-org-ol-doc))
             ('treemacs-org-ol-parent-section-open-state (treemacs-collapse-org-ol-parent-section))
             ('treemacs-org-ol-parent-section-closed-state (treemacs-expand-org-ol-parent-section)))
         (org-ol-tree-navigation--visit-current)))
      (treemacs--evade-image))))

(defun org-ol-tree-input--doubleclick-action (event)
  "Visit the clicked heading from EVENT.

This function cancels any timer call from `org-ol-tree-input--leftclick-action'."
  (interactive "e")

  (when org-ol-tree-input--debounce-timer
    (cancel-timer org-ol-tree-input--debounce-timer)
    (setq org-ol-tree-input--debounce-timer nil))

  (when (eq 'double-mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))
    (when (region-active-p)
      (keyboard-quit))
    (org-ol-tree-navigation--visit-current)))


;;; Navigation

(defun org-ol-tree-navigation--collapse-current ()
  "Collapse an expandable section that is currently expanded.

If the cursor is not on top of an expanded section, calling this function has
no effect."
  (interactive)
  (treemacs-with-current-button
   "No heading node found under the cursor"
   (pcase (treemacs-button-get current-btn :state)
     ('treemacs-org-ol-doc-open-state (treemacs-collapse-org-ol-doc))
     ('treemacs-org-ol-parent-section-open-state (treemacs-collapse-org-ol-parent-section)))))


(defun org-ol-tree-navigation--expand-current ()
  "Expand an expandable section that is currently collapsed.

If the cursor is not on top of a collapsed section, calling this function has
no effect."
  (interactive)
  (treemacs-with-current-button
   "No heading node found under the cursor"
   (pcase (treemacs-button-get current-btn :state)
     ('treemacs-org-ol-doc-closed-state (treemacs-expand-org-ol-doc))
     ('treemacs-org-ol-parent-section-closed-state (treemacs-expand-org-ol-parent-section)))))


(defun org-ol-tree-navigation--visit-current (&optional narrow-p &rest _)
  "Switch to the buffer saved in node at point.

When this function is invoked with a prefix argument, NARROW-P is set to a
non-nil value and it toggles the narrowed state. For instance, if your buffer
is not narrowed, invoking this function with a prefix argument causes the
selected section to get narrowed. From now on, subsequent calls of this feature
narrow the selected section, until you call it with th universal argument
again, which causes the buffer to get widen."
  (interactive "P")

  (if-let ((buffer (and (bound-and-true-p org-ol-tree--org-buffer)
                        (buffer-live-p org-ol-tree--org-buffer)
                        org-ol-tree--org-buffer))
           (window (get-buffer-window))
           (doc-window (or (get-buffer-window org-ol-tree--org-buffer)
                           (next-window)))
           (heading (org-ol-tree-heading-current))
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

(org-ol-tree-icons-update-theme)

(treemacs-define-leaf-node org-ol-section 'dynamic-icon
                           :ret-action #'org-ol-tree-navigation--visit-current
                           :mouse1-action #'org-ol-tree-navigation--visit-current)


(treemacs-define-expandable-node org-ol-parent-section
  :icon-open-form (org-ol-tree-icons-section-icon (treemacs-button-get node :heading) 'expanded)
  :icon-closed-form (org-ol-tree-icons-section-icon (treemacs-button-get node :heading) 'collapsed)
  :ret-action 'org-ol-tree-navigation--visit-current
  :query-function (reverse (org-ol-tree-heading-subheadings (treemacs-button-get node :heading)))
  :render-action
  (treemacs-render-node :icon (org-ol-tree-icons-section-icon item 'collapsed)
                        :label-form (org-ol-tree-heading-name item)
                        :state (if (org-ol-tree-heading-subheadings item)
                                   treemacs-org-ol-parent-section-closed-state
                                 treemacs-org-ol-section-state)
                        :key-form (org-ol-tree-heading-id item)
                        :face 'treemacs-file-face
                        :more-properties (:heading item)))


(treemacs-define-expandable-node org-ol-doc
  :icon-open (org-ol-tree-icons-root-icon)
  :icon-closed (org-ol-tree-icons-root-icon)
  :ret-action 'org-ol-tree-navigation--visit-current
  :query-function (reverse (org-ol-tree-heading-subheadings (org-ol-tree-heading-root)))
  :top-level-marker t
  :root-face 'treemacs-root-face
  :root-key-form (org-ol-tree-heading-id (org-ol-tree-heading-root))
  :root-label (org-ol-tree-heading-name (org-ol-tree-heading-root))
  :render-action
  (treemacs-render-node :icon (org-ol-tree-icons-section-icon item 'collapsed)
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
         (window (display-buffer-in-side-window buffer `((side . ,org-ol-tree-window-position)))))
    ;; (set-window-margins window 2 2)
    (select-window window)
    (treemacs-initialize)
    (setq-local org-ol-tree--org-buffer origin-buffer
                treemacs--width-is-locked nil
                window-size-fixed nil
                treemacs-mode-map nil)

    (setq header-line-format '("☰ Outline"))
    (set-window-buffer nil (current-buffer))
    (treemacs-ORG-OL-DOC-extension)
    (treemacs-expand-org-ol-doc)
    (beginning-of-line)
    (org-ol-tree-mode 1)))


;;;; ---- Mode definition

(define-minor-mode org-ol-tree-mode "Treemacs generic mode."
  nil nil org-ol-tree-mode-map)

(provide 'org-ol-tree)
;;; org-ol-tree.el ends here
