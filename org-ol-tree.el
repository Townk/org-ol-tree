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
;; Package-Requires: ((org "9.5") (treemacs "2.8") (dash "2.18.1") (s "1.12.0") (seq) (cl-lib))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; For an overview information about the package with a quick start,
;; installation and usages, refer to the README.org file distributed along with
;; this file.
;;
;; This file is splitted into 8 major sections with each one having its own
;; number of subsections. I chose this organization to force me to keep a good
;; separation between the package major components.
;;
;; This is the list of all sections and its respective subsections:
;;
;; - Variables
;;   - Private variables
;;   - Configuration variables
;; - System information
;; - Outline elements
;;   - Sections
;;   - Headings
;; - Outline visuals (UI)
;;   - Icons
;;   - Window
;; - Node actions
;; - Treemacs extension
;; - Commands
;; - Mode definition
;;
;; These sections are also used to separate their unit test files.
;;
;; Regarding namespacing, the package uses the `org-ol-tree' prefix as namespace
;; and each section adds its own namespace to it. For instance, the section
;; "System information", has the full namespace as `org-ol-tree-system'. The two
;; exceptions to this rule are Sections and Heading that are actually
;; subsections but got their very own namespace due to the fact thay are of
;; higher importance in the package (and also because function names look better
;; without the 'elements' prefix).
;;
;;; Code:

(require 'org)
(require 'treemacs)
(require 'dash)
(require 's)
(require 'seq)
(require 'subr-x)

(require 'all-the-icons nil 'noerror)
(require 'evil nil 'noerror)


;;;; --- Variables

;;; --- Private variables -------------------------------------------------------

(defvar-local org-ol-tree--org-buffer nil
  "A buffer local variable used to hold the buffer object where the outline
should act on.")


(defvar-local org-ol-tree-heading--root nil
  "Hold the root node for the displayed outline.")


(defvar-local org-ol-tree-heading--root-dirt-p nil
  "Flag indicating the root hading needs to be rebuild.")


(defvar-local org-ol-tree-input--debounce-timer nil
  "The timer waiting to debounce click operations on the tree view.")

(defvar-local org-ol-tree-window--current-width 0
  "The timer waiting to debounce click operations on the tree view.")

(defvar org-ol-tree-icons--selected-theme nil
  "This variable holds the current icon theme used by the outline.

To know how this variable is populated, check the `org-ol-tree-icons-theme'
documentation.

Never update this variable manually. It is intended to self-mutate when calling
the `org-ol-tree-visuals-update-icons-theme' function.")


;;; --- Configuration variables -------------------------------------------------

(defvar org-ol-tree-window-position 'right
  "Symbol indicating where to open the outline window.

Usually, the value of this variable is `left' or `right'.")


(defvar org-ol-tree-window-max-width 0.4
  "Define the outline window maximum width.

If the value is a float between 0 and 1, the maximum width is given by
multiplying this value and the maximum available size for the window.

If this value is an integer greater or equal 1, the maximum size is the given
value. If the option `org-ol-tree-window-use-pixel' is non nil, the given value
is consider to be in pixels.

If the given value does not fall into these two categories, its assumed you want
the maximum width to be the size of the maximum available size. If it does, the
value will also be capped between `window-min-width' and the maximum available
size.")


(defvar org-ol-tree-window-min-width 0.2
  "Define the outline window minimum width.

If the value is a float between 0 and 1, the minimum width is given by
multiplying this value and the maximum available size for the window.

If this value is an integer greater or equal 1, the minimum size is the given
value itself. If the option `org-ol-tree-window-use-pixel' is non nil, the given
value is consider to be in pixels.

If the given value does not fall into these two categories, its assumed you want
the minimum width to be the size of `window-min-width'. If it does, the value
will also be capped between `window-min-width' and the maximum available size.")


(defvar org-ol-tree-window-use-pixel t
  "Flag indicating if window measurements should be in pixels.

This flag will only be used on graphical frames, and it is useful if you have
any `variable-pitch' among outline faces.")


(defvar org-ol-tree-window-auto-resize t
  "Indicates the outline window should adjust its size to show its content.

When this option is nil, `org-ol-tree-window-max-width' is used as the outline
window absolute size.")


(defvar org-ol-tree-window-increase-only nil
  "If set to a non nil value, only grow the size of the window.

This behavior is applied during window resize, which happens when the window
configuration for the outline window changes, or when use expands or collapses
the nodes.")


(defvar org-ol-tree-icons-theme-plist (list)
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
displayed as the `display' property of a two-characters propertized string. This
is done to guarantee the proper alignment of icons when using unicode or
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

3) If the Emacs frame is a graphical frame, and the package all-the-icons is NOT
   installed nor available, the theme `unicode' is used;

4) Fallback to the `ascii' theme;")



;;;; --- System information

;; Functions in this section are inline because I want to make sure the state
;; returned by them is as accurate as possible.

(defsubst org-ol-tree-system-all-the-icons-p ()
  "Constant indicating if package all-the-icons is installed."
  (fboundp 'all-the-icons-material))


(defsubst org-ol-tree-system-evil-p ()
  "Constant indicating if package evil is installed."
  (and (fboundp 'evil-define-key)
       (fboundp 'evil-window-top)
       (fboundp 'evil-window-middle)
       (fboundp 'evil-window-bottom)))

(defsubst org-ol-tree-system-graphical-frame-p ()
  "Return t if current frame is a GUI frame, nil otherwise.

To find out if Emacs is running in GUI mode, we query the variable
`window-system'."
  (member window-system '(x w32 ns)))



;;;; --- Core objects

;;; --- Sections ---------------------------------------------------------------

(defun org-ol-tree-section-p (stack-or-string)
  "Return t when STACK-OR-STRING is a valid section object.

If STACK-OR-STRING is a list, all its elements should be numbers representing
individual numbers from a section id as on the reverse order do they appear. For
instance, the section \"1.3.2 My section text\" would be represented as (2 3 1)
on the stack form.

If STACK-OR-STRING is a string it should have only integers separated by dots as
they would appear on a section heading. For instance, the same \"1.3.2 My
section text\" heading is represented by the string \"1.3.2\"."
  (and stack-or-string
       (or (and (stringp stack-or-string)
                (string-match-p "^[0-9]+\\(\\.[0-9]+\\)*$" stack-or-string))
           (and (listp stack-or-string)
                (seq-every-p 'number-or-marker-p stack-or-string)))))


(defun org-ol-tree-section-stack-to-string (section-stack)
  "Convert a list of numbers into a section number string notation.

This function does the conversion by transforming each element from
SECTION-STACK into a string, reversing the list, and joining all elements with a
'.' character.

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


;;; --- Headings ---------------------------------------------------------------

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
  (level 0
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
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot create an org-ol-tree-heading on a non-org buffer"))

  (unless (org-at-heading-p)
    (user-error "Cannot create an org-ol-tree-heading with cursor outside an actual org headline"))

  (unless (or (null previous-heading)
              (org-ol-tree-heading-p previous-heading))
    (error "Given parent must be nil or an 'org-ol-tree-heading' object"))

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
                                                   :id "0"
                                                   :marker (point-min-marker)
                                                   :level 0))
                current-heading)
            (while (outline-next-heading)
              (let ((this-heading (org-ol-tree-heading-create current-heading)))
                (unless (org-ol-tree-heading-parent this-heading)
                  (push this-heading (org-ol-tree-heading-subheadings root)))
                (setq current-heading this-heading)))
            root))))))


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


(defun org-ol-tree-heading-root ()
  "Return the root heading for the outline in the current window.

The root heading is cached on a buffer local variable"
  (unless (and (bound-and-true-p org-ol-tree-heading--root) (not org-ol-tree-heading--root-dirt-p))
    (when (bound-and-true-p org-ol-tree--org-buffer)
      (setq-local org-ol-tree-heading--root
                  (org-ol-tree-heading-root-build org-ol-tree--org-buffer))
      (setq-local org-ol-tree-heading--root-dirt-p nil)))
  org-ol-tree-heading--root)


(defun org-ol-tree-heading-current ()
  "Return the heading object for the tree node under the cursor.

If cursor is outside a heading node, return nil."
  (treemacs-with-current-button
   "No heading node found under the cursor"
   (or (treemacs-button-get current-btn :heading)
       (when (= (line-number-at-pos) 1) (org-ol-tree-heading-root)))))



;;;; --- Outline visuals (UI)

;;; --- Icons -------------------------------------------------------------------

(defun org-ol-tree-visuals-update-icons-theme ()
  "Refresh `org-ol-tree-icons--selected-theme' values.

To know how this function populate the `org-ol-tree-icons--selected-theme',
check the `org-ol-tree-icons-theme' variable documentation."
  (setq org-ol-tree-icons--selected-theme
        (plist-get org-ol-tree-icons-theme-plist
                   (cond
                    ((member org-ol-tree-icons-theme org-ol-tree-icons-theme-plist)
                     org-ol-tree-icons-theme)
                    ((and (org-ol-tree-system-graphical-frame-p)
                          (org-ol-tree-system-all-the-icons-p))
                     'all-the-icons)
                    ((org-ol-tree-system-graphical-frame-p)
                     'unicode)
                    (t 'assystem-feature)))))

(defun org-ol-tree-visuals-root-icon ()
  "Return the string used as the icon for the root element."
  (let* ((root-icon (plist-get org-ol-tree-icons--selected-theme :root))
         (display-p (> (length root-icon) 0)))
    (concat
     " "
     (when display-p (propertize "--" 'face 'treemacs-root-face 'display root-icon))
     (when display-p " "))))


(defun org-ol-tree-visuals-section-icon (heading state)
  "Return the full icon for the giving HEADING.

The icon depends on the icon theme configuration as well as the expandable state
of HEADING.

The STATE argument indicates if this icon should represent an open or closed
node.Valid values for STATE are 'expanded,'collapsed, and nil. In practice, this
function considers the state as 'collapsed for any value non nil and different
than 'expanded."
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


;; I define the icon themes here because 1) this is the UI section and the Icons
;; subsection, and 2) we want all functions related to icons to be defined
;; before we define the actual icons.

(setq org-ol-tree-icons-theme-plist
      (-non-nil
       (append '()
               (when (org-ol-tree-system-all-the-icons-p)
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
               (list 'unicode `(:root "■"                 ; <-- I really don't like
                                :expanded "▾ "            ;     this icon, please
                                :collapsed "▸ "           ;     give me a better
                                :section "§ %(section)")  ;     suggestion!
                     'ascii `(:root "*"
                              :expanded "- "
                              :collapsed "+ "
                              :section "%(section)"))
               (when (org-ol-tree-system-all-the-icons-p)
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
                                 :section "")))))

;;; --- Window ------------------------------------------------------------------

(defun org-ol-tree-visuals-use-pixel-p ()
  "Return t if the measurement unit should be pixels.

This function takes in account the value of `org-ol-tree-window-use-pixel' and
if this frame is a graphical frame or not."
  (and (org-ol-tree-system-graphical-frame-p)
       org-ol-tree-window-use-pixel))


(defun org-ol-tree-visuals--window-do-resize (window target-width min-width max-width
                                                     total-width char-width pixelwise)
  "The actual resize function.

This function is called by `org-ol-tree-visuals-window-resize' with all the
calculated values in place to perform the window resizing.

The calculated values needed here are as follows:

- WINDOW: The window where the resize should happen;
- TARGET-WIDTH: The desired width for the window;
- MIN-WIDTH: The lower bound constraint for the window width;
- MAX-WIDTH: The higher bound constraint for the window width;
- TOTAL-WIDTH: The total available width to increase the window;
- CHAR-WIDTH: The width of a character on the current face of current frame;
- PIXELWISE: Indicates if the resize should happen in pixels (t value) or
  columns (nil value)"
  (unless pixelwise
    (setq target-width (/ (+ target-width char-width -1) char-width)))

  (setq target-width (max min-width (min max-width target-width)))

  (unless (= target-width total-width)
    (window-preserve-size window t)
    (window-resize-no-error
     window (- target-width total-width) t window pixelwise)
    (when org-ol-tree-window-increase-only
      (setq-local org-ol-tree-window--current-width target-width))))


(defun org-ol-tree-visuals-window-resize ()
  "Adjust the window width to fit the outline content as much as possible.

The adjustment respects the value of `org-ol-tree-window-max-width' (check its
documentation for more details).

The majority of the code in this function was copied from the Emacs function
`fit-window-to-buffer'."
  (with-selected-window (selected-window)
    (let* ((window (window-normalize-window (selected-window) t))
           (frame (window-frame window))
           (pixelwise (org-ol-tree-visuals-use-pixel-p))
           (char-width (frame-char-width frame))
           (total-width (window-size window t pixelwise))
           (available-width (+ total-width
                               (window-max-delta window t window nil t nil pixelwise)))
           (max-width (cond
                       ((<= org-ol-tree-window-max-width 0)
                        available-width)
                       ((integerp org-ol-tree-window-max-width)
                        (min org-ol-tree-window-max-width available-width))
                       ((> org-ol-tree-window-max-width 1) available-width)
                       (t (min (round (* org-ol-tree-window-max-width (float available-width)))
                               available-width)))))
      (if org-ol-tree-window-auto-resize
          (let* ((char-height (frame-char-height frame))
                 (min-width (max (if pixelwise
                                     (* char-width window-min-width)
                                   window-min-width)
                                 (window-min-size window nil window pixelwise)))
                 (min-width (cond
                             ((<= org-ol-tree-window-min-width 0)
                              min-width)
                             ((integerp org-ol-tree-window-min-width)
                              (max org-ol-tree-window-min-width min-width))
                             ((> org-ol-tree-window-max-width 1.0)
                              min-width)
                             (t
                              (max (round (* org-ol-tree-window-min-width (float available-width)))
                                     min-width))))
                 (min-width (max min-width org-ol-tree-window--current-width))
                 (width (+ (+ (car (window-text-pixel-size
                                    window (window-start window) nil
                                    (frame-pixel-width (window-frame window))
                                    (* (window-body-height window pixelwise)
                                       (if pixelwise 1 char-height))))
                              (- total-width
                                 (window-body-width window pixelwise)))
                           (* char-width 2)))
                 (window-size-fixed nil))
            (org-ol-tree-visuals--window-do-resize window width min-width max-width
                                                   total-width char-width pixelwise))
        (org-ol-tree-visuals--window-do-resize window max-width max-width max-width
                                               total-width char-width pixelwise)))))



;;;; --- Node actions

(defun org-ol-tree-actions--leftclick (event)
  "Function used to perform a mouse click on a node.

The action triggered by this function depends if the node is a leaf or an
expandable node. If leaf, it will trigger the `treemacs-RET-action', and if it
is an expandable node, it will trigger the `treemacs-TAB-action'. A potential
prefix EVENT is passed on to the executed action, if possible."
  (interactive "e")

  (when org-ol-tree-input--debounce-timer
    (cancel-timer org-ol-tree-input--debounce-timer))

  (setq org-ol-tree-input--debounce-timer
        (run-with-idle-timer 0.1 nil #'org-ol-tree-actions--expand-or-visit event)))


(defun org-ol-tree-actions--expand-or-visit (event)
  "Helper function called by `org-ol-tree-actions--leftclick'.

This function performs the actual action for the click operation.

The argument EVENT, is the same event received by the
`org-ol-tree-actions--leftclick' function."
  (setq org-ol-tree-input--debounce-timer nil)

  (when (eq 'mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))
    (goto-char (point-at-bol))

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
         (org-ol-tree-actions--visit)))
      (treemacs--evade-image))))


(defun org-ol-tree-actions--doubleclick (event)
  "Visit the clicked heading from EVENT.

This function cancels any timer call from `org-ol-tree-actions--leftclick'."
  (interactive "e")

  (when org-ol-tree-input--debounce-timer
    (cancel-timer org-ol-tree-input--debounce-timer)
    (setq org-ol-tree-input--debounce-timer nil))

  (when (eq 'double-mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))
    (goto-char (point-at-bol))
    (when (region-active-p)
      (keyboard-quit))
    (org-ol-tree-actions--visit)))


(defun org-ol-tree-actions--goto-root ()
  "Move the cursor to the outline root node."
  (interactive)
  (treemacs-goto-node '(:custom "0")))


(defun org-ol-tree-actions--goto-last-node ()
  "Move the cursor to the last opened node.

If the last node on the tree is a parent node with several children, but its
state is collapsed, the parent not will be selected."
  (interactive)
  (goto-char (point-max))
  (goto-char (point-at-bol)))


(defun org-ol-tree-actions--collapse ()
  "Collapse an expandable section that is currently expanded.

If the cursor is not on top of an expanded section, calling this function has no
effect."
  (interactive)
  (treemacs-with-current-button
   "No heading node found under the cursor"
   (pcase (treemacs-button-get current-btn :state)
     ('treemacs-org-ol-doc-open-state (treemacs-collapse-org-ol-doc))
     ('treemacs-org-ol-parent-section-open-state (treemacs-collapse-org-ol-parent-section)))))


(defun org-ol-tree-actions--expand ()
  "Expand an expandable section that is currently collapsed.

If the cursor is not on top of a collapsed section, calling this function has no
effect."
  (interactive)
  (treemacs-with-current-button
   "No heading node found under the cursor"
   (pcase (treemacs-button-get current-btn :state)
     ('treemacs-org-ol-doc-closed-state (treemacs-expand-org-ol-doc))
     ('treemacs-org-ol-parent-section-closed-state (treemacs-expand-org-ol-parent-section)))))


(defun org-ol-tree-actions--visit (&optional narrow-p &rest _)
  "Switch to the buffer saved in node at point.

When this function is invoked with a prefix argument, NARROW-P is set to a
non-nil value and it toggles the narrowed state. For instance, if your buffer is
not narrowed, invoking this function with a prefix argument causes the selected
section to get narrowed. From now on, subsequent calls of this feature narrow
the selected section, until you call it with th universal argument again, which
causes the buffer to get widen."
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



;;;; --- Treemacs extension

(org-ol-tree-visuals-update-icons-theme)

(treemacs-define-leaf-node org-ol-section 'dynamic-icon
                           :ret-action #'org-ol-tree-actions--visit
                           :mouse1-action #'org-ol-tree-actions--visit)


(treemacs-define-expandable-node org-ol-parent-section
  :icon-open-form (org-ol-tree-visuals-section-icon (treemacs-button-get node :heading) 'expanded)
  :icon-closed-form (org-ol-tree-visuals-section-icon (treemacs-button-get node :heading)
                                                      'collapsed)
  :ret-action 'org-ol-tree-actions--visit
  :after-expand (org-ol-tree-visuals-window-resize)
  :after-collapse (org-ol-tree-visuals-window-resize)
  :query-function (reverse (org-ol-tree-heading-subheadings (treemacs-button-get node :heading)))
  :render-action
  (treemacs-render-node :icon (org-ol-tree-visuals-section-icon item 'collapsed)
                        :label-form (org-ol-tree-heading-name item)
                        :state (if (org-ol-tree-heading-subheadings item)
                                   treemacs-org-ol-parent-section-closed-state
                                 treemacs-org-ol-section-state)
                        :key-form (org-ol-tree-heading-id item)
                        :face 'treemacs-file-face
                        :more-properties (:heading item)))


(treemacs-define-expandable-node org-ol-doc
  :icon-open (org-ol-tree-visuals-root-icon)
  :icon-closed (org-ol-tree-visuals-root-icon)
  :ret-action 'org-ol-tree-actions--visit
  :after-expand (org-ol-tree-visuals-window-resize)
  :after-collapse (org-ol-tree-visuals-window-resize)
  :query-function (reverse (org-ol-tree-heading-subheadings (org-ol-tree-heading-root)))
  :top-level-marker t
  :root-face 'treemacs-root-face
  :root-key-form (org-ol-tree-heading-id (org-ol-tree-heading-root))
  :root-label (org-ol-tree-heading-name (org-ol-tree-heading-root))
  :render-action
  (treemacs-render-node :icon (org-ol-tree-visuals-section-icon item 'collapsed)
                        :label-form (org-ol-tree-heading-name item)
                        :state treemacs-org-ol-parent-section-closed-state
                        :key-form (org-ol-tree-heading-id item)
                        :face 'treemacs-file-face
                        :more-properties (:heading item)))



;;;; --- Commands

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
                org-ol-tree-window--current-width 0)

    (setq header-line-format '("☰ Outline"))
    (set-window-buffer nil (current-buffer))
    (treemacs-ORG-OL-DOC-extension)
    (treemacs-expand-org-ol-doc)
    (add-hook 'window-configuration-change-hook 'org-ol-tree-visuals-window-resize nil t)
    (beginning-of-line)
    (save-excursion
      (read-only-mode -1)
      (goto-char (point-max))
      (insert "\n")
      (read-only-mode 1))
    (org-ol-tree-mode 1)))


;;;; --- Mode definition

(define-minor-mode org-ol-tree-mode
  "Org Outline Tree mode."
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1]        #'org-ol-tree-actions--leftclick)
             (define-key map [double-mouse-1] #'org-ol-tree-actions--doubleclick)
             (define-key map (kbd "<right>")  #'org-ol-tree-actions--expand)
             (define-key map (kbd "C-f")      #'org-ol-tree-actions--expand)
             (define-key map (kbd "<left>")   #'org-ol-tree-actions--collapse)
             (define-key map (kbd "C-b")      #'org-ol-tree-actions--collapse)
             (define-key map (kbd "<home>")   #'org-ol-tree-actions--goto-root)
             (define-key map (kbd "C-a")      #'org-ol-tree-actions--goto-root)
             (define-key map (kbd "<end>")    #'org-ol-tree-actions--goto-last-node)
             (define-key map (kbd "C-e")      #'org-ol-tree-actions--goto-last-node)

             ;; ignore treemacs bindings

             (define-key map (kbd "<C-?>") 'ignore)
             (define-key map (kbd "<M-UP>") 'ignore)
             (define-key map (kbd "<M-DOWN>") 'ignore)
             (define-key map (kbd "<M-!>") 'ignore)
             (define-key map "\C-c\C-p" 'ignore)
             (define-key map "\C-c\C-w" 'ignore)
             (define-key map "\\!" 'ignore)
             (define-key map "!" 'ignore)
             (define-key map "?" 'ignore)
             (define-key map "P" 'ignore)
             (define-key map "R" 'ignore)
             (define-key map "b" 'ignore)
             (define-key map "c" 'ignore)
             (define-key map "d" 'ignore)
             (define-key map "gr" 'ignore)
             (define-key map "m" 'ignore)
             (define-key map "o" 'ignore)
             (define-key map "r" 'ignore)
             (define-key map "s" 'ignore)
             (define-key map "t" 'ignore)
             (define-key map "w" 'ignore)
             (define-key map "y" 'ignore)
             map))


(when (org-ol-tree-system-evil-p)
  (evil-define-key 'treemacs 'org-ol-tree-mode
    "l"             #'org-ol-tree-actions--expand
    (kbd "<right>") #'org-ol-tree-actions--expand
    "h"             #'org-ol-tree-actions--collapse
    (kbd "<left>")  #'org-ol-tree-actions--collapse
    "gg"            #'org-ol-tree-actions--goto-root
    (kbd "<home>")  #'org-ol-tree-actions--goto-root
    "G"             #'org-ol-tree-actions--goto-last-node
    (kbd "<end>")   #'org-ol-tree-actions--goto-last-node
    "H" #'(lambda () (interactive) (evil-window-top) (goto-char (point-at-bol)))
    "M" #'(lambda () (interactive) (evil-window-middle) (goto-char (point-at-bol)))
    "L" #'(lambda () (interactive) (evil-window-bottom) (goto-char (point-at-bol)))
    "0" #'(lambda () (interactive) (goto-char (point-at-bol)))
    "^" #'(lambda () (interactive) (goto-char (point-at-bol)))
    "$" #'(lambda () (interactive) (goto-char (point-at-bol)))

    ;; ignore treemacs bindings

    (kbd "<C-?>") 'ignore
    (kbd "<M-UP>") 'ignore
    (kbd "<M-DOWN>") 'ignore
    (kbd "<M-!>") 'ignore
    "\C-c\C-p" 'ignore
    "\C-c\C-w" 'ignore
    "\\!" 'ignore
    "!" 'ignore
    "?" 'ignore
    "P" 'ignore
    "R" 'ignore
    "b" 'ignore
    "c" 'ignore
    "d" 'ignore
    "gr" 'ignore
    "m" 'ignore
    "o" 'ignore
    "r" 'ignore
    "s" 'ignore
    "t" 'ignore
    "w" 'ignore
    "y" 'ignore))

(provide 'org-ol-tree)
;;; org-ol-tree.el ends here
