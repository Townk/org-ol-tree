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
;; Package-Requires: ((org "9.4") (treemacs "2.8") (dash "2.18.1") (s "1.12.0") (ht "2.3") (seq) (cl-lib))
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
;; separation between the package major components. These sections are also used
;; to separate their unit test files.
;;
;; For namespaces the package uses the `org-ol-tree' prefix and each section
;; adds its own namespace to it.
;;
;; This is the list of all sections with their namespace indicated between
;; parenthesis, and its respective subsections:
;;
;; - Variables
;;   - Private variables
;;   - Configuration variables
;;   - Constants
;; - System information (system)
;; - Core objects (core)
;;   - Sections
;;   - Headlines
;;   - Document
;; - UI (ui)
;;   - Icons
;;   - Window
;;   - Buffer
;; - Node actions (actions)
;; - Treemacs extension
;; - Mode definition
;; - Commands
;;
;; Because each variable has its namespaces defined by the section that uses it,
;; the "Variables" section has no namespace defined. The "Treemacs extension"
;; and "Mode definition" don't have namespaces either because those are pure
;; configuration sections. And in the end, the "Commands" section uses the main
;; namespace for itself.
;;
;; Another important convention I use in this file is the "double-dash" for
;; privates. This means that if you see a function or variable with a "--"
;; separating its namespace from its name, this function or variable are meant
;; exclusively for the internals of this package.
;;
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; * *                                                                     * *
;; * *   The private functions and private variables are subject to        * *
;; * *   change without any warning, even after a stable release. So do    * *
;; * *   yourself a favor and DO NOT USE THEM in your config.              * *
;; * *                                                                     * *
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'org)
(require 's)
(require 'seq)
(require 'subr-x)
(require 'treemacs)

(require 'all-the-icons nil 'noerror)
(require 'evil nil 'noerror)


;;;; --- Faces

(defface org-ol-tree-document-face
  '((t :inherit treemacs-root-face))
  "Face used by org-ol-tree to display the root node."
  :group 'org-ol-tree-faces)


(defface org-ol-tree-section-title-face
  '((t :inherit font-lock-doc-face))
  "Face used by org-ol-tree to display section titles."
  :group 'org-ol-tree-faces)


(defface org-ol-tree-section-id-face
  '((t :inherit treemacs-file-face))
  "Face used by org-ol-tree to display section titles."
  :group 'org-ol-tree-faces)



;;;; --- Variables

;; After reading about sections in this file and their respective namespaces,
;; you might ask why am I defining the package's variables on the top of the
;; file? There are 2 reasons for it:
;;
;; 1) When you open this file, besides the information on th header, th first
;;    thing I want you to see, are what you can and can't customize on your
;;    personal config file;
;;
;; 2) I still have some C in me and my first reaction when I declare variables
;;    on any programming language I use, is to put them on the top of the scope,
;;    so I used reason #1 to not change an old habit;

;;; --- Private variables -------------------------------------------------------

(defvar-local org-ol-tree--buffer-p nil
  "Flag indicating if current buffer is an org-ol-tree buffer.")


(defvar-local org-ol-tree--buffer nil
  "The org-ol-tree buffer associated with the current Org file.

This variable is only set on Org files when user displays the outline.

Never set or modify this variable directly.")


(defvar-local org-ol-tree--org-buffer nil
  "The Outline buffer displaying the tree view.")


(defvar-local org-ol-tree-action--debounce-timer nil
  "The timer waiting to debounce click operations on the tree view.")


(defvar-local org-ol-tree-ui--window-width 0
  "The timer waiting to debounce click operations on the tree view.")


(defvar org-ol-tree-ui--icon-set nil
  "This variable holds the current icon set used by the outline.

To know how this variable is populated, check the `org-ol-tree-ui-icon-set'
documentation.

Never update this variable manually. It is intended to self-mutate when calling
the `org-ol-tree-ui--update-icon-set' function.")


(defvar org-ol-tree-action--buffer-watchers (ht-create)
  "A hash table that associates watcher description and their outline buffer.")


(defvar org-ol-tree-action--watcher-buffers (ht-create)
  "A hash table that associates org buffers and their file watchers.")


;;; --- Configuration variables -------------------------------------------------

(defvar org-ol-tree-action-move-to-target nil
  "When non nil, move cursor to the selected header on the org file.

If nil, after selecting a header on the Outline window, show the header and move
the cursor back to the Outline window.")


(defvar org-ol-tree-action-close-on-selected nil
  "When non nil, close the Outline window after selecting a header.")


(defvar org-ol-tree-ui-window-position 'right
  "Symbol indicating where to open the outline window.
Possible values for this variable are `left' or `right'.")


(defvar org-ol-tree-ui-window-max-width 0.4
  "Define the outline window maximum width.

If the value is a float between 0 and 1, the maximum width is given by
multiplying this value and the maximum available size for the window.

If this value is an integer greater or equal 1, the maximum size is the given
value. If the option `org-ol-tree-ui-window-use-pixel' is non nil, the given
value is consider to be in pixels.

If the given value does not fall into these two categories, its assumed you want
the maximum width to be the size of the maximum available size. If it does, the
value will also be capped between `window-min-width' and the maximum available
size.")


(defvar org-ol-tree-ui-window-min-width 0.2
  "Define the outline window minimum width.

If the value is a float between 0 and 1, the minimum width is given by
multiplying this value and the maximum available size for the window.

If this value is an integer greater or equal 1, the minimum size is the given
value itself. If the option `org-ol-tree-ui-window-use-pixel' is non nil, the
given value is consider to be in pixels.

If the given value does not fall into these two categories, its assumed you want
the minimum width to be the size of `window-min-width'. If it does, the value
will also be capped between `window-min-width' and the maximum available size.")


(defvar org-ol-tree-ui-window-use-pixel t
  "Flag indicating if window measurements should be in pixels.

This flag will only be used on graphical frames, and it is useful if you have
any `variable-pitch' among outline faces.")


(defvar org-ol-tree-ui-window-auto-resize t
  "Indicates the outline window should adjust its size to show its content.

When this option is nil, `org-ol-tree-ui-window-max-width' is used as the
outline window absolute size.")


(defvar org-ol-tree-ui-window-increase-only nil
  "If set to a non nil value, only grow the size of the window.

This behavior is applied during window resize, which happens when the window
configuration for the outline window changes, or when use expands or collapses
the nodes.")


(defvar org-ol-tree-ui-window-header-format '(" ☰ Outline")
  "Control the Outline Header's appearance.

If this variable is nil, org-ol-tree won't display a header on the outline
window. If its value should be the same type accepted by `modeline' and
`headerline', or a function. If th value is a function, before displaying the
header, org-ol-tree evaluates the function and uses the result as header. This
result must be in the format required by `modeline' and `headerline'")


(defvar org-ol-tree-ui-icon-set-list (list)
  "A property list of property list representing all icon themes available.

An icon theme is a simple property list with four entries:

  `:root' - The root node icon for the outline, displayed on the left of the
            document's title;
  `:expanded' - The icon displayed on the left of expandable nodes that are
                currently expanded;
  `:collapsed' - The icon displayed on the left of expandable nodes that are
                 currently collapsed;
  `:sectiion' - The icon displayed on the left side of all section headline on
                the outline;

All icons are strings. The `:root', `:expanded', and `collapsed' icons are
displayed with the `display' property of a two-characters propertized string.
This is done to guarantee the proper alignment of icons when using unicode or
all-the-icons icons.

The `:section' icon is actually a simple one-line string where we replace the
string \"%(section)\" by the section number of the headline. Different then the
other icons, the `:section' icon allows any arbitrary size string.")


(defvar org-ol-tree-ui-icon-set nil
  "The theme to use on the outline icons.

To check all available themes, look into `org-ol-tree-ui-icon-set-list'
documentation.

The outline chooses the theme based on the following criteria:

1) If this variable is not nil, and the symbol from it is one of the available
   themes, the theme indicated by the variable is used;

2) If the Emacs frame is a graphical frame, and the package all-the-icons is
   installed and available, the theme `all-the-icons' is used;

3) If the Emacs frame is a graphical frame, and the package all-the-icons is NOT
   installed nor available, the theme `unicode' is used;

4) Fallback to the `ascii' theme;")



;;; --- Constants ---------------------------------------------------------------

(defconst org-ol-tree-ui--buffer-prefix "*OrgOutlineTree"
  "String prefixing all org-ol-tree buffers.")


(defconst org-ol-tree-core--headline-re
  (concat "\\(.*\\)[[:blank:]]+"
          "\\(\\[[[:digit:]]*[[:blank:]]*/[[:blank:]]*[[:digit:]]*[[:blank:]]*\\]\\)"
          "[[:blank:]]*$")
  "Regular expression to match task progress on an Org headline.")


(defconst org-ol-tree-core--title-re
  "^#\\+title:[ \t]*\\([^\n]+\\)"
  "Regular expression to match an Org document title.")



;;;; --- System information

(defun org-ol-tree-system--all-the-icons-p ()
  "Constant indicating if package all-the-icons is installed."
  (fboundp 'all-the-icons-material))


(defun org-ol-tree-system--evil-p ()
  "Constant indicating if package evil is installed."
  (and (fboundp 'evil-define-key)
        (fboundp 'evil-window-top)
        (fboundp 'evil-window-middle)
        (fboundp 'evil-window-bottom)))

(defun org-ol-tree-system--graphical-frame-p ()
  "Return t if current frame is a GUI frame, nil otherwise.

To find out if Emacs is running in GUI mode, we query the variable
`window-system'."
  (member window-system '(x w32 ns)))



;;;; --- Core objects

;;; --- Sections ---------------------------------------------------------------

(defun org-ol-tree-core--section-p (stack-or-string)
  "Return t when STACK-OR-STRING is a valid section object.

If STACK-OR-STRING is a list, all its elements should be numbers representing
individual numbers from a section id as on the reverse order do they appear. For
instance, the section \"1.3.2 My section text\" would be represented as (2 3 1)
on the stack form.

If STACK-OR-STRING is a string it should have only integers separated by dots as
they would appear on a section headline. For instance, the same \"1.3.2 My
section text\" headline is represented by the string \"1.3.2\"."
  (and stack-or-string
       (or (and (stringp stack-or-string)
                (string-match-p "^[0-9]+\\(\\.[0-9]+\\)*$" stack-or-string))
           (and (listp stack-or-string)
                (seq-every-p 'number-or-marker-p stack-or-string)))))


(defun org-ol-tree-core--section-string (section-stack)
  "Convert a list of numbers into a section number string notation.

This function does the conversion by transforming each element from
SECTION-STACK into a string, reversing the list, and joining all elements with a
'.' character.

Example::
  (org-ol-tree-core--section-string '(3 2 1))

  ;; => \"1.2.3\""
  (when (org-ol-tree-core--section-p section-stack)
    (string-join (reverse (mapcar 'number-to-string section-stack)) ".")))


(defun org-ol-tree-core--section-from-string (section-string)
  "Convert SECTION-STRING into a section stack.

Example::
  (org-ol-tree-core--section-from-string \"1.2.3\")

  ;; => (3 2 1)"
  (when (org-ol-tree-core--section-p section-string)
    (reverse (mapcar 'string-to-number (split-string section-string "\\.")))))


(defun org-ol-tree-core--next-section (section target-level)
  "Return a new section-stack for the next SECTION on TARGET-LEVEL.

For more information on the meaning of a section-stack, look the
`org-ol-tree-core--section-p' documentation.

Examples::
  (org-ol-tree-core--next-section '(3 2 1) 3)  ;; => (4 2 1)
  (org-ol-tree-core--next-section '(3 2 1) 2)  ;; => (3 1)
  (org-ol-tree-core--next-section '(3 2 1) 4)  ;; => (1 3 2 1)"

  (if (or (not section)
          (and (stringp section) (string-empty-p section)))
      (-repeat target-level 1)

    (unless (org-ol-tree-core--section-p section)
      (error "The given section object is not an org-ol-tree-section"))

    (let* ((section (if (stringp section) (org-ol-tree-core--section-from-string section) section))
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


;;; --- Headlines ---------------------------------------------------------------

(defun org-ol-tree-headline--split-progress (text)
  "Return a cons cell with headline title and progress mark.

TEXT is the full headline title where this function will search for the progress
mark ([/])."
  (if (string-match org-ol-tree-core--headline-re text)
      `(,(s-trim (match-string 1 text)) . ,(s-trim (match-string 2 text)))
    `(,text . nil)))


(cl-defstruct (org-ol-tree-core--headline (:constructor org-ol-tree-core--headline-create-internal)
                                          (:copier nil)
                                          :noinline)
  "The Org Outline Tree headline structure.

It has the basic information to build and draw a tree-like structure
representing an entire org document."
  (name nil
        :type string
        :documentation "The org headline text with no decorations.")
  (progress nil
            :type string
            :documentation "A string indicating progress on this headline tasks.")
  (id nil
      :type string
      :documentation "A string representing the section number.")
  (marker nil
          :type marker
          :documentation "Location of this headline on its org file buffer.")
  (level 0
         :type number
         :documentation "The nested level for this org headline.")
  (parent nil
          :type org-ol-tree-core--headline
          :documentation "The parent headline or nil if this is a root headline.")
  (children nil
            :type list
            :documentation "A collection of children headlines."))


(defun org-ol-tree-core--create-headline (&optional previous-headline)
  "Create a new `org-ol-tree-core--headline' from `point' on current org buffer.

If PREVIOUS-HEADLINE is non nil, this function creates the new headline as a
child of the given parent.

If `current-buffer' is not an org buffer, or `point' is not over an org headline,
this functions raises a user error."
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot create an org-ol-tree-core--headline on a non-org buffer"))

  (unless (org-at-heading-p)
    (user-error "Cannot create a headline with cursor outside an actual org headline"))

  (unless (or (null previous-headline)
              (org-ol-tree-core--headline-p previous-headline))
    (error "Given parent must be nil or an 'org-ol-tree-core--headline' object"))

  (let* ((previous-level (if (null previous-headline)
                             0
                           (org-ol-tree-core--headline-level previous-headline)))
         (previous-id (when (org-ol-tree-core--headline-p previous-headline)
                        (org-ol-tree-core--headline-id previous-headline)))
         (this-components (org-heading-components))
         (this-name (nth 4 this-components))
         (this-name (org-ol-tree-headline--split-progress this-name))
         (this-progress (cdr this-name))
         (this-name (car this-name))
         (this-level (nth 0 this-components))
         (this-marker (point-marker))
         (this-parent (cond
                       ((= this-level previous-level)
                        (org-ol-tree-core--headline-parent previous-headline))
                       ((> this-level previous-level) previous-headline)
                       ((< this-level previous-level)
                        (let ((node previous-headline)
                              (node-level previous-level))
                          (while (and (not (null node))
                                      (<= this-level node-level))
                            (setq node (org-ol-tree-core--headline-parent node))
                            (if (null node)
                                (setq node-level 0)
                              (setq node-level (org-ol-tree-core--headline-level node))))
                          node))
                       (t nil)))
         (this-section-stack (org-ol-tree-core--next-section previous-id this-level))
         (this-section-id (org-ol-tree-core--section-string this-section-stack))
         (this-headline (org-ol-tree-core--headline-create-internal :name this-name
                                                                  :progress this-progress
                                                                  :id this-section-id
                                                                  :marker this-marker
                                                                  :level this-level
                                                                  :parent this-parent)))
    (when this-parent
      (push this-headline (org-ol-tree-core--headline-children this-parent)))
    this-headline))


(defun org-ol-tree-core--root-headline ()
  "Return the headline object for the root node of the tree."
  (org-ol-tree-core--node-get :headline (org-ol-tree-core--root-node)))


(defun org-ol-tree-core--current-headline ()
  "Return the headline object for the tree node under the cursor.

If cursor is outside a headline node, return nil."
  (org-ol-tree-core--node-get :headline))


;;; --- Treemacs integration ---------------------------------------------------

(defun org-ol-tree-core--root-path ()
  "Return the treemacs path to the root node."
  '(:custom root))


(defun org-ol-tree-core--section-path (&optional section-id skip-root)
  "Return the path list for the given SECTION-ID.

If SECTION-ID is nil, uses the headline id of current node.

If SKIP-ROOT is non nil, it will not include the initial path of
`org-ol-tree-core--root-path'."
  (append (unless skip-root (org-ol-tree-core--root-path))
          (-reduce-from
           (lambda (acc-list elm)
             (-snoc acc-list (mapconcat 'identity (-snoc (last acc-list) elm) ".")))
           nil
           (split-string
            (or section-id (org-ol-tree-core--headline-id (org-ol-tree-core--current-headline)))
            "\\."))))


(defun org-ol-tree-core--root-node ()
  "Return the treemacs button for the root node of the tree."
  (save-excursion
    (goto-char (treemacs-project->position (treemacs-project-at-point)))
    (org-ol-tree-core--current-node)))


(defun org-ol-tree-core--current-node ()
  "Wrapper function around `treemacs-current-button' to allow mocks."
  (treemacs-current-button))


(defun org-ol-tree-core--node-get (property &optional node)
  "Wrapper function around `treemacs-button-get' to allow mocks.

Return the value of PROPERTY associated with NODE."
  (treemacs-button-get (or node (org-ol-tree-core--current-node)) property))


(defun org-ol-tree-core--node-put (property value &optional node)
  "Wrapper function around `treemacs-button-put' to allow mocks.

Set the VALUE of PROPERTY associated with NODE."
  (treemacs-button-put (or node (org-ol-tree-core--current-node)) property value))


;;; --- Document ---------------------------------------------------------------

(defun org-ol-tree-core--create-dom (&optional buffer-or-name)
  "Traverse BUFFER-OR-NAME buffer to create a tree-like structure for headlines.

This function uses the `outline-next-heading' function to traverse the org file
and uses the cl-struct `org-ol-tree-core--headline' as node information.

If BUFFER-OR-NAME is nil, uses the `current-buffer'. If the given buffer is not
an Org buffer, raises a `user-error'."
  (let ((buffer (if buffer-or-name
                    (get-buffer buffer-or-name)
                  (or org-ol-tree--org-buffer
                      (current-buffer)))))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (user-error "Can't traverse an Org document on a NON-Org buffer"))

      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((root (org-ol-tree-core--headline-create-internal
                       :id "0"
                       :marker (point-min-marker)
                       :level 0))
                current-headline)
            (while (outline-next-heading)
              (let ((this-headline (org-ol-tree-core--create-headline current-headline)))
                (unless (org-ol-tree-core--headline-parent this-headline)
                  (push this-headline (org-ol-tree-core--headline-children root)))
                (setq current-headline this-headline)))
            root))))))


(defun org-ol-tree-core--find-title (&optional buffer-or-name)
  "Return a string label for the outline root node.

The label is given by the title on the BUFFER-OR-NAME buffer if one is defined,
by the file name of the target buffer transformed to title case, if the target
buffer has a file associated with it, or by the target's buffer name
transformed to title case."
  (let ((buffer (if buffer-or-name
                    (get-buffer buffer-or-name)
                  (or org-ol-tree--org-buffer
                      (current-buffer))))
        (title-limit (* 1024 10)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (cond
         ((re-search-forward org-ol-tree-core--title-re title-limit t)
          (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
         ((buffer-file-name)
          (s-titleized-words (file-name-base (buffer-file-name))))
         (t (s-titleized-words (buffer-name))))))))



;;;; --- UI

;;; --- Icons -------------------------------------------------------------------

(defun org-ol-tree-ui--update-icon-set ()
  "Refresh `org-ol-tree-ui--icon-set' values.

To know how this function populate the `org-ol-tree-ui--icon-set',
check the `org-ol-tree-ui-icon-set' variable documentation."
  (setq org-ol-tree-ui--icon-set
        (plist-get org-ol-tree-ui-icon-set-list
                   (cond
                    ((member org-ol-tree-ui-icon-set org-ol-tree-ui-icon-set-list)
                     org-ol-tree-ui-icon-set)
                    ((and (org-ol-tree-system--graphical-frame-p)
                          (org-ol-tree-system--all-the-icons-p))
                     'all-the-icons)
                    ((org-ol-tree-system--graphical-frame-p)
                     'unicode)
                    (t 'ascii)))))


(defun org-ol-tree-ui--use-fancy-icons-p ()
  "Return t if the selected icon set is one of the `all-the-icons' set."
  (and (org-ol-tree-system--graphical-frame-p)
       (org-ol-tree-system--all-the-icons-p)
       (or (not org-ol-tree-ui-icon-set)
           (member org-ol-tree-ui-icon-set '(all-the-icons iconless-fancy)))))


(defun org-ol-tree-ui--expand-collapse-icon (headline state)
  "Return the string used for the collapse or expand symbol on sections.

If the HEADLINE used to get this icon does not have children, this function
returns two white spaces used to align with the collapsable headlines.

The STATE argument indicates if this icon should represent an open or closed
node.Valid values for STATE are 'expanded,'collapsed, and nil. In practice, this
function considers the state as 'collapsed for any value non nil and different
than 'expanded."
  (if (org-ol-tree-core--headline-children headline)
      (let ((expanded-icon (plist-get org-ol-tree-ui--icon-set :expanded))
            (collapsed-icon (plist-get org-ol-tree-ui--icon-set :collapsed)))
        (if (org-ol-tree-ui--use-fancy-icons-p)
            (propertize "--"
                        'face 'org-ol-tree-section-title-face
                        'display (if (eq state 'expanded) expanded-icon collapsed-icon))
          (propertize (if (eq state 'expanded) expanded-icon collapsed-icon)
                      'face 'org-ol-tree-section-title-face)))
    "  "))


(defun org-ol-tree-ui--doc-icon ()
  "Return the string used as the icon for the root element."
  (let* ((doc-icon (plist-get org-ol-tree-ui--icon-set :root))
         (display-p (> (length doc-icon) 0)))
    (concat
     " "
     (when display-p (if (org-ol-tree-ui--use-fancy-icons-p)
                         (propertize "--" 'face 'org-ol-tree-document-face 'display doc-icon)
                       (propertize doc-icon 'face 'org-ol-tree-document-face)))
     (when display-p " "))))


(defun org-ol-tree-ui--section-icon (headline state)
  "Return the full icon for the giving HEADLINE.

The icon depends on the icon theme configuration as well as the given STATE
of the HEADLINE."
  (let ((section-icon (plist-get org-ol-tree-ui--icon-set :section)))
  (concat
   " "
   (org-ol-tree-ui--expand-collapse-icon headline state)
   (when (> (length section-icon) 0)
     (propertize
      (format "%s " (s-replace "%(section)" (org-ol-tree-core--headline-id headline) section-icon))
      'face
      'org-ol-tree-section-title-face)))))


;; I define the icon themes here because 1) this is the UI section and the Icons
;; subsection, and 2) we want all functions related to icons to be defined
;; before we define the actual icons.

(setq org-ol-tree-ui-icon-set-list
      (-non-nil
       (append '()
               (when (org-ol-tree-system--all-the-icons-p)
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
               (when (org-ol-tree-system--all-the-icons-p)
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

(defun org-ol-tree-ui--setup-window (create-window)
  "Display the Outline buffer on a side window.

If CREATE-WINDOW is a non nil value, this function creates a side window and
displays the Outline buufer into it."
  (if (not create-window)
      (select-window (get-buffer-window org-ol-tree--buffer))
    (-> org-ol-tree--buffer
      (display-buffer-in-side-window `((side . ,org-ol-tree-ui-window-position)))
      (select-window)
      (set-window-dedicated-p t))
    (add-hook 'window-configuration-change-hook 'org-ol-tree-ui--window-resize nil t)))


(defun org-ol-tree-ui--get-window ()
  "Return the window displaying the org-ol-tree buffer for the current org file.

Returns nil if no org-ol-tree buffer is visible."
  (if org-ol-tree--buffer-p
      (selected-window)
    (when (buffer-live-p org-ol-tree--buffer)
      (get-buffer-window org-ol-tree--buffer))))


(defun org-ol-tree-ui--visibility ()
  "Return whether the current visibility state of the org-ol-tree buffer.
Valid states are 'visible, 'exists and 'none."
  (cond
   ((org-ol-tree-ui--get-window) 'visible)
   ((buffer-live-p org-ol-tree--buffer) 'exists)
   (t 'none)))


(defun org-ol-tree-ui--use-pixel-p ()
  "Return t if the measurement unit should be pixels.

This function takes in account the value of `org-ol-tree-ui-window-use-pixel'
and if this frame is a graphical frame or not."
  (and (org-ol-tree-system--graphical-frame-p)
        org-ol-tree-ui-window-use-pixel))


(defun org-ol-tree-ui--window-system-min-width (window char-width pixelwise)
  "Return real minimal width for WINDOW.

If PIXELWISE is non nil, this function uses CHAR-WIDTH to calculate the minimal
width in pixels.

The real minimal width is given by the variable `window-min-width' and the
function `window-min-size' (which can potentially differ), whichever is bigger."
  (max (if pixelwise (* char-width window-min-width) window-min-width)
       (window-min-size window nil window pixelwise)))


(defun org-ol-tree-ui--window-min-width (available-width system-min-width)
  "Return the minimum possible width for window.

This function uses AVAILABLE-WIDTH and SYSTEM-MIN-WIDTH to calculate the
minimum width from `org-ol-tree-ui-window-min-width'.

For mor information on SYSTEM-MIN-WIDTH check
`org-ol-tree-ui--window-system-min-width' documentation."
  (cond
   ((<= org-ol-tree-ui-window-min-width 0) system-min-width)
   ((integerp org-ol-tree-ui-window-min-width)
    (max org-ol-tree-ui-window-min-width system-min-width))
   ((> org-ol-tree-ui-window-max-width 1.0) system-min-width)
   (t (max (round (* org-ol-tree-ui-window-min-width (float available-width)))
           system-min-width))))


(defun org-ol-tree-ui--window-max-width (available-width)
  "Return the maximum possible width for window.

This function uses AVAILABLE-WIDTH as the maximum size the window can take in
the frame when `org-ol-tree-ui-window-max-width' is a ration rather then a
number."
  (cond
   ((<= org-ol-tree-ui-window-max-width 0) available-width)
   ((integerp org-ol-tree-ui-window-max-width) (min org-ol-tree-ui-window-max-width
                                                    available-width))
   ((>= org-ol-tree-ui-window-max-width 1) available-width)
   (t (min (round (* org-ol-tree-ui-window-max-width (float available-width)))
           available-width))))


(defun org-ol-tree-ui--window-required-width (window current-width pixelwise)
  "Calculates the minimal width of WINDOW to display its entire buffer.

This function uses the CURRENT-WIDTH to calculate WINDOW's fringes, margins,
scroll bars, and its right divider, if any. If PIXELWISE is non nil, this
calculation uses pixels instead of characters."
  (+ (car (window-text-pixel-size window
                                  (window-start window) nil
                                  (frame-pixel-width (window-frame window)) nil))
     (- current-width (window-body-width window pixelwise))))


(defun org-ol-tree-ui--window-pad-width (width char-width pixelwise)
  "Return WIDTH with the necessary padding for the Outline.

This function uses the given CHAR-WIDTH to pad its width.

If PIXELWISE is non nil, this calculation uses pixels instead of characters."
  (let ((width (+ width (* char-width 2))))
    (if pixelwise
        width
      (/ (+ width char-width -1) char-width))))


(defun org-ol-tree-ui--window-perform-resize (window current-width new-width pixelwise)
  "Adjust the size of WINDOW to NEW-WIDTH if it's different than CURRENT-WIDTH.

If PIXELWISE is non nil, perform the resize using pixels instead of characters."
  (unless (= new-width current-width)
    (window-preserve-size window t)
    (window-resize-no-error
     window (- new-width current-width) t window pixelwise)
    (when org-ol-tree-ui-window-increase-only
      (setq-local org-ol-tree-ui--window-width new-width))))


(defun org-ol-tree-ui--window-resize ()
  "Adjust the window width to fit the outline content as much as possible.

The adjustment respects the value of `org-ol-tree-ui-window-max-width' (check
its documentation for more details).

The majority of the code in this function was copied from the Emacs function
`fit-window-to-buffer'."
  (let* ((pixelwise (org-ol-tree-ui--use-pixel-p))
         (char-width (frame-char-width))
         (window (window-normalize-window (selected-window) t))
         (current-width (window-size window t pixelwise))
         (max-delta (window-max-delta window t window nil t nil pixelwise))
         (available-width (+ current-width max-delta))
         (system-min-width (org-ol-tree-ui--window-system-min-width window char-width pixelwise))
         (max-width (org-ol-tree-ui--window-max-width available-width))
         (min-width (org-ol-tree-ui--window-min-width available-width system-min-width))
         (min-width (max min-width org-ol-tree-ui--window-width))
         (required-width (org-ol-tree-ui--window-required-width window current-width pixelwise))
         (required-width (org-ol-tree-ui--window-pad-width required-width char-width pixelwise))
         (target-width (max min-width (min max-width required-width)))
         (window-size-fixed nil))
    (org-ol-tree-ui--window-perform-resize window
                                           current-width
                                           (if org-ol-tree-ui-window-auto-resize
                                               target-width
                                             max-width)
                                           pixelwise)))


;;; --- Buffer ------------------------------------------------------------------

(defun org-ol-tree-ui--get-buffer-create (name)
  "Retrieve or create an org-ol-tree buffer with NAME for current Org buffer."
  (if (buffer-live-p org-ol-tree--buffer)
      org-ol-tree--buffer
    (get-buffer-create (format "%s:%s*" org-ol-tree-ui--buffer-prefix name))))


(defun org-ol-tree-ui--setup-buffer ()
  "Create and setup a buffer for Org Outline Tree."
  (if-let ((origin-buffer (current-buffer))
           (org-buffer-p (eq major-mode 'org-mode))
           (buffer (org-ol-tree-ui--get-buffer-create (buffer-name))))
      (progn
        (add-hook 'kill-buffer-hook 'org-ol-tree-action--quit-on-kill nil t)
        (setq-local org-ol-tree--buffer buffer)
        (with-current-buffer buffer
            (unless org-ol-tree--buffer-p
              (treemacs-initialize)
              (setq-local org-ol-tree--org-buffer origin-buffer
                          org-ol-tree--buffer-p t
                          treemacs--width-is-locked nil
                          window-size-fixed nil
                          org-ol-tree-ui--window-width 0)
              (setq header-line-format (if (functionp org-ol-tree-ui-window-header-format)
                                           (funcall org-ol-tree-ui-window-header-format)
                                         org-ol-tree-ui-window-header-format))
              (org-ol-tree-mode 1))))
    (error "Can't use Org Outline Tree on non-Org buffers")))


(defun org-ol-tree-ui--kill-buffer ()
  "Kill the org-ol-tree buffer."
  (interactive)
  (when org-ol-tree--buffer-p
    ;; teardown logic handled in kill hook
    (if (one-window-p)
        (kill-this-buffer)
      (kill-buffer-and-window))))


(defun org-ol-tree-ui--build-outline-tree ()
  "Erase and re-draw the entire tree hierarchy for the current Outline."
  (let ((buffer (if org-ol-tree--buffer-p (current-buffer) org-ol-tree--buffer)))
    (unless buffer
      (user-error "Cannot rebuild the Outline tree from an unrelated buffer"))

    (with-current-buffer buffer
      (treemacs-with-writable-buffer
       (erase-buffer)
       (treemacs-ORG-OL-DOC-extension)
       (org-ol-tree-core--node-put :headline (org-ol-tree-core--create-dom)
                                   (org-ol-tree-core--root-node))
       (treemacs-expand-org-ol-doc)
       (save-excursion
         (goto-char (point-max))
         (insert "\n"))))))



;;;; --- Node actions

;;; --- Mouse -------------------------------------------------------------------

(defun org-ol-tree-action--leftclick (event)
  "Function used to perform a mouse click on a node.

The action triggered by this function depends if the node is a leaf or an
expandable node. If leaf, it will trigger the `treemacs-RET-action', and if it
is an expandable node, it will trigger the `treemacs-TAB-action'. A potential
prefix EVENT is passed on to the executed action, if possible."
  (interactive "e")

  (when org-ol-tree-action--debounce-timer
    (cancel-timer org-ol-tree-action--debounce-timer))

  (setq org-ol-tree-action--debounce-timer
        (run-with-idle-timer 0.1 nil #'org-ol-tree-action--expand-or-visit event)))


(defun org-ol-tree-action--expand-or-visit (event)
  "Helper function called by `org-ol-tree-action--leftclick'.

This function performs the actual action for the click operation.

The argument EVENT, is the same event received by the
`org-ol-tree-action--leftclick' function."
  (setq org-ol-tree-action--debounce-timer nil)

  (when (eq 'mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))
    (goto-char (point-at-bol))

    (when (region-active-p)
      (keyboard-quit))

    (when-let ((headline (org-ol-tree-core--current-headline)))
      (if (org-ol-tree-core--headline-children headline)
          (pcase (org-ol-tree-core--node-get :state)
            ('treemacs-org-ol-doc-open-state (treemacs-collapse-org-ol-doc))
            ('treemacs-org-ol-doc-closed-state (treemacs-expand-org-ol-doc))
            ('treemacs-org-ol-parent-section-open-state (treemacs-collapse-org-ol-parent-section))
            ('treemacs-org-ol-parent-section-closed-state (treemacs-expand-org-ol-parent-section)))
        (org-ol-tree-action--visit)))))


(defun org-ol-tree-action--doubleclick (event)
  "Visit the clicked headline from EVENT.

This function cancels any timer call from `org-ol-tree-action--leftclick'."
  (interactive "e")

  (when org-ol-tree-action--debounce-timer
    (cancel-timer org-ol-tree-action--debounce-timer)
    (setq org-ol-tree-action--debounce-timer nil))

  (when (eq 'double-mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))
    (goto-char (point-at-bol))
    (when (region-active-p)
      (keyboard-quit))
    (org-ol-tree-action--visit)))


;;; --- Navigation --------------------------------------------------------------

(defun org-ol-tree-action--goto-setion (section-id)
  "Move the cursor to the section with id SECTION-ID.

This function expands any node necessary to reach the proper section. If a
section with the given SECTION-ID does not exists, an `user-error' is raised."
  (let ((current-path (org-ol-tree-core--root-path))
        (target-path (org-ol-tree-core--section-path section-id t)))
    (while target-path
      (setq current-path (append current-path (list (pop target-path))))
      (treemacs-goto-node current-path)))
  (goto-char (point-at-bol)))


(defun org-ol-tree-action--goto-root ()
  "Move the cursor to the outline root node."
  (interactive)
  (treemacs-goto-node (org-ol-tree-core--root-path)))


(defun org-ol-tree-action--goto-last-node ()
  "Move the cursor to the last opened node.

If the last node on the tree is a parent node with several children, but its
state is collapsed, the parent not will be selected."
  (interactive)
  (goto-char (point-max))
  (goto-char (point-at-bol)))


(defun org-ol-tree-action--collapse ()
  "Collapse an expandable section that is currently expanded.

If the cursor is not on top of an expanded section, calling this function has no
effect."
  (interactive)
  (pcase (org-ol-tree-core--node-get :state)
    ('treemacs-org-ol-doc-open-state (treemacs-collapse-org-ol-doc))
    ('treemacs-org-ol-parent-section-open-state (treemacs-collapse-org-ol-parent-section))))


(defun org-ol-tree-action--expand ()
  "Expand an expandable section that is currently collapsed.

If the cursor is not on top of a collapsed section, calling this function has no
effect."
  (interactive)
  (pcase (org-ol-tree-core--node-get :state)
    ('treemacs-org-ol-doc-closed-state (treemacs-expand-org-ol-doc))
    ('treemacs-org-ol-parent-section-closed-state (treemacs-expand-org-ol-parent-section))))


(defun org-ol-tree-action--move-to (target-point)
  "Move the cursor to TARGET-POINT and scroll point to top.

If the buffer is narrowed, it will get widen as a side effect of this function."
  (widen)
  (goto-char target-point)
  (org-reveal)
  (org-show-entry)
  (recenter (min (max 0 scroll-margin) (truncate (/ (window-body-height) 4.0))) t))


;;; --- Visit -------------------------------------------------------------------

(defun org-ol-tree-action--narrow-buffer-maybe (perform-narrrowing narrowed-p)
  "Narrow current Org buffer according to PERFORM-NARRROWING and NARROWED-P."
  (when (or (and (not perform-narrrowing) narrowed-p)
            (and perform-narrrowing (not narrowed-p)))
    (org-narrow-to-subtree)))


(defun org-ol-tree-action--focus-on-headline (narrow-p)
  "Focus this Org buffer on the current headline.

If NARROW-P is non nil, it toggles the narrowed state. For instance, if your
buffer is not narrowed, invoking this function with a prefix argument causes
the selected section to get narrowed. From now on, subsequent calls of this
feature narrow the selected section, until you call it with th universal
argument again, which causes the buffer to get widen."
  (when-let ((buffer (and (buffer-live-p org-ol-tree--org-buffer)
                          org-ol-tree--org-buffer))
             (doc-window (or (get-buffer-window org-ol-tree--org-buffer)
                             (next-window)))
             (headline (org-ol-tree-core--current-headline))
             (headline-marker (org-ol-tree-core--headline-marker headline)))
        (select-window doc-window)
        (switch-to-buffer buffer)
        (let ((current-narrow (buffer-narrowed-p)))
          (org-ol-tree-action--move-to headline-marker)
          (org-ol-tree-action--narrow-buffer-maybe narrow-p current-narrow)
          t)))


(defun org-ol-tree-action--visit (&optional narrow-p &rest _)
  "Switch to the buffer saved in node at point.

When this function is invoked with a prefix argument, NARROW-P is set to a
non-nil value and it toggles the narrowed state. For instance, if your buffer is
not narrowed, invoking this function with a prefix argument causes the selected
section to get narrowed. From now on, subsequent calls of this feature narrow
the selected section, until you call it with th universal argument again, which
causes the buffer to get widen."
  (interactive "P")

  (unless (org-ol-tree-action--focus-on-headline narrow-p)
    (user-error "No section information found on current point"))

  (when (or (not org-ol-tree-action-move-to-target) org-ol-tree-action-close-on-selected)
          (select-window (org-ol-tree-ui--get-window))
          (when org-ol-tree-action-close-on-selected
            (org-ol-tree-action--quit)))
        (treemacs-pulse-on-success))


;;; --- Rename ------------------------------------------------------------------

(defun org-ol-tree-action--rename-title (new-title)
  "Rename current Org document title to NEW-TITLE.

If the current document does not have a title defined by '#+TITLE:', a
`user-error' is raised."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward org-ol-tree-core--title-re nil t)
      (progn
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1) (match-end 1))
        (insert new-title))
      (goto-char (point-min))
      (insert (format "#+TITLE: %s\n" new-title)))))


(defun org-ol-tree-action--rename-section (headline-point todo-progress new-name)
  "Rename a section on HEADLINE-POINT to NEW-NAME.

TODO-PROGRESS is the text defining the progress of a multi-task TODO at the end
of the headline. For instance, on the section \"Section Title [2/3]\", the
TODO-PROGRESS should be \"[2/3]\"."
  (save-excursion
    (goto-char headline-point)
    (org-edit-headline (concat
                        new-name
                        (when todo-progress
                          (format " %s" todo-progress))))))


(defun org-ol-tree-action--rename (&optional headline new-title current-name)
  "Rename HEADLINE to NEW-TITLE.

If the cursor is on top of the root node, the rename will change thee document
title.

If CURRENT-NAME is given, this function does not retrieve the node current name
for comparison and uses the provided name instead."
  (interactive
   (let* ((headline (org-ol-tree-core--current-headline))
          (current-name (org-ol-tree-core--headline-name headline)))
     (list headline (read-string "New name: " current-name) current-name)))
  (let ((marker (org-ol-tree-core--headline-marker headline))
        (root-headline (org-ol-tree-core--root-headline))
        (progress (org-ol-tree-core--headline-progress headline)))
    (when (not (equal current-name new-title))
      (setf (org-ol-tree-core--headline-name headline) new-title)
      (if (eq headline root-headline)
          (progn
            (org-ol-tree-ui--build-outline-tree)
            (with-current-buffer org-ol-tree--org-buffer
              (org-ol-tree-action--rename-title new-title)))
        (with-current-buffer org-ol-tree--org-buffer
          (org-ol-tree-action--rename-section marker progress new-title)))
      (org-ol-tree-action--refresh t))))


;;; --- Refresh -----------------------------------------------------------------

(defun org-ol-tree-action--refresh (&optional prevent-rebuild target-section-id)
  "Refresh the Outline tree.

If PREVENT-REBUILD is non nil, this function just refresh the buffer content
without refreshing the base data.

If TARGET-SECTION-ID is not nil, uses it instead of the current headline id."
  (interactive)

  (when org-ol-tree--buffer-p
    (let ((current-section-id
           (or target-section-id
               (org-ol-tree-core--headline-id (org-ol-tree-core--current-headline)))))
      (unless prevent-rebuild
        (org-ol-tree-ui--build-outline-tree))

      (org-ol-tree-action--goto-root)
      (treemacs-collapse-org-ol-doc)
      (org-ol-tree-action--goto-setion current-section-id))))


(defun org-ol-tree-action--filewatch-callback (event)
  "Function called when the operating system detects a file change.

The file watched is always `org-ol-tree--org-buffer'.

For more information on EVENT, check the documentation of
`file-notify-add-watch'."

  (save-restriction
    (widen)
    (save-excursion
      (cl-multiple-value-bind (descriptor action file) event
        (when (member action '(renamed changed))
          (let ((ol-buffer (ht-get org-ol-tree-action--watcher-buffers  descriptor))
                (current-window (selected-window)))
            (if (and ol-buffer (buffer-live-p ol-buffer))
                (progn
                  (when (eq (org-ol-tree-ui--visibility) 'visible)
                    (select-window (get-buffer-window ol-buffer))
                    (org-ol-tree-action--refresh)
                    (select-window current-window)))
              (ht-remove! org-ol-tree-action--watcher-buffers descriptor)
              (ht-remove! org-ol-tree-action--buffer-watchers ol-buffer)
              (file-notify-rm-watch descriptor))))))))



(defun org-ol-tree-action--start-watching-buffer ()
  "Set a file-watcher for the Org buffer associated with this Outline."

  (when org-ol-tree--buffer-p
    (when-let ((org-file (buffer-file-name org-ol-tree--org-buffer))
               (watcher (file-notify-add-watch org-file
                                               '(change)
                                               #'org-ol-tree-action--filewatch-callback)))
      (ht-set! org-ol-tree-action--buffer-watchers (current-buffer) watcher)
      (ht-set! org-ol-tree-action--watcher-buffers watcher (current-buffer)))))


(defun org-ol-tree-action--stop-watching-buffer ()
  "Remove the file-watcher for the Org buffer associated with this Outline."

  (when org-ol-tree--buffer-p
    (when-let ((watcher (ht-get org-ol-tree-action--buffer-watchers (current-buffer)))
               (buffer (ht-get org-ol-tree-action--watcher-buffers watcher)))
      (unless (eq buffer (current-buffer))
          (error "Problem matching file watcher with current Outline"))
      (ht-remove! org-ol-tree-action--watcher-buffers watcher)
      (ht-remove! org-ol-tree-action--buffer-watchers buffer)
      (file-notify-rm-watch watcher))))


;;; --- Setup / Tear down -------------------------------------------------------

(defun org-ol-tree-action--init()
  "Initialize an org-ol-tree for the current Org buffer."
  (org-ol-tree-ui--setup-buffer)
  (org-ol-tree-ui--setup-window t)
  (org-ol-tree-ui--build-outline-tree)
  (org-ol-tree-action--start-watching-buffer)
  (beginning-of-line))


(defun org-ol-tree-action--quit-on-kill ()
  "Hook function used to kill the Outline window when killing the Org buffer."
  (when org-ol-tree--buffer
    (pcase (org-ol-tree-ui--visibility)
      ('visible
       (select-window (get-buffer-window org-ol-tree--buffer))
       (org-ol-tree-action--stop-watching-buffer)
       (call-interactively 'org-ol-tree-ui--kill-buffer))
      ('exists
       (kill-buffer org-ol-tree--buffer)))))


(defun org-ol-tree-action--quit (&optional arg)
  "Quit org-ol-tree with `bury-buffer'.

With a prefix ARG call `org-ol-tree-ui--kill-buffer' instead."
  (interactive "P")
  (when (or org-ol-tree--buffer-p (buffer-live-p org-ol-tree--buffer))
    (with-current-buffer (or (when (buffer-live-p org-ol-tree--buffer) org-ol-tree--buffer)
                             (current-buffer))
      (if arg
          (org-ol-tree-ui--kill-buffer)
        (bury-buffer)))))



;;;; --- Treemacs extension

(org-ol-tree-ui--update-icon-set)

(treemacs-define-leaf-node org-ol-section 'dynamic-icon
                           :ret-action #'org-ol-tree-action--visit
                           :mouse1-action #'org-ol-tree-action--visit)


(treemacs-define-expandable-node org-ol-parent-section
  :icon-open-form (org-ol-tree-ui--section-icon (org-ol-tree-core--node-get :headline node)
                                                'expanded)
  :icon-closed-form (org-ol-tree-ui--section-icon (org-ol-tree-core--node-get :headline node)
                                                  'collapsed)
  :ret-action 'org-ol-tree-action--visit
  :after-expand (org-ol-tree-ui--window-resize)
  :after-collapse (org-ol-tree-ui--window-resize)
  :query-function (reverse
                   (org-ol-tree-core--headline-children
                    (org-ol-tree-core--node-get :headline node)))
  :render-action
  (treemacs-render-node :icon (org-ol-tree-ui--section-icon item 'collapsed)
                        :label-form (org-ol-tree-core--headline-name item)
                        :state (if (org-ol-tree-core--headline-children item)
                                   treemacs-org-ol-parent-section-closed-state
                                 treemacs-org-ol-section-state)
                        :key-form (org-ol-tree-core--headline-id item)
                        :face 'org-ol-tree-section-id-face
                        :more-properties (:headline item)))


(treemacs-define-expandable-node org-ol-doc
  :icon-open (org-ol-tree-ui--doc-icon)
  :icon-closed (org-ol-tree-ui--doc-icon)
  :ret-action 'org-ol-tree-action--visit
  :after-expand (org-ol-tree-ui--window-resize)
  :after-collapse (org-ol-tree-ui--window-resize)
  :query-function (reverse
                   (org-ol-tree-core--headline-children
                    (org-ol-tree-core--node-get :headline node)))
  :top-level-marker t
  :root-face 'org-ol-tree-document-face
  :root-key-form (car (last (org-ol-tree-core--root-path)))
  :root-label (org-ol-tree-core--find-title)
  :render-action
  (treemacs-render-node :icon (org-ol-tree-ui--section-icon item 'collapsed)
                        :label-form (org-ol-tree-core--headline-name item)
                        :state treemacs-org-ol-parent-section-closed-state
                        :key-form (org-ol-tree-core--headline-id item)
                        :face 'org-ol-tree-section-id-face
                        :more-properties (:headline item)))



;;;; --- Mode definition

(define-minor-mode org-ol-tree-mode
  "Org Outline Tree mode."
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1]        #'org-ol-tree-action--leftclick)
             (define-key map [double-mouse-1] #'org-ol-tree-action--doubleclick)
             (define-key map (kbd "<right>")  #'org-ol-tree-action--expand)
             (define-key map (kbd "C-f")      #'org-ol-tree-action--expand)
             (define-key map (kbd "<left>")   #'org-ol-tree-action--collapse)
             (define-key map (kbd "C-b")      #'org-ol-tree-action--collapse)
             (define-key map (kbd "<home>")   #'org-ol-tree-action--goto-root)
             (define-key map (kbd "C-a")      #'org-ol-tree-action--goto-root)
             (define-key map (kbd "<end>")    #'org-ol-tree-action--goto-last-node)
             (define-key map (kbd "C-e")      #'org-ol-tree-action--goto-last-node)
             (define-key map "q"              #'org-ol-tree-action--quit)
             (define-key map "R"              #'org-ol-tree-action--rename)
             (define-key map "r"              #'org-ol-tree-action--refresh)


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
             (define-key map "b" 'ignore)
             (define-key map "c" 'ignore)
             (define-key map "d" 'ignore)
             (define-key map "gr" 'ignore)
             (define-key map "m" 'ignore)
             (define-key map "o" 'ignore)
             (define-key map "s" 'ignore)
             (define-key map "t" 'ignore)
             (define-key map "w" 'ignore)
             (define-key map "y" 'ignore)
             map))


(when (org-ol-tree-system--evil-p)
  (evil-define-key 'treemacs 'org-ol-tree-mode
    "l"             #'org-ol-tree-action--expand
    (kbd "<right>") #'org-ol-tree-action--expand
    "h"             #'org-ol-tree-action--collapse
    (kbd "<left>")  #'org-ol-tree-action--collapse
    "gg"            #'org-ol-tree-action--goto-root
    (kbd "<home>")  #'org-ol-tree-action--goto-root
    "G"             #'org-ol-tree-action--goto-last-node
    (kbd "<end>")   #'org-ol-tree-action--goto-last-node
    "q" #'org-ol-tree-action--quit
    "R" #'org-ol-tree-action--rename
    "r" #'org-ol-tree-action--refresh
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
    "b" 'ignore
    "c" 'ignore
    "d" 'ignore
    "gr" 'ignore
    "m" 'ignore
    "o" 'ignore
    "s" 'ignore
    "t" 'ignore
    "w" 'ignore
    "y" 'ignore))



;;;; --- Commands

;;;###autoload
(defun org-ol-tree ()
  "Initialise or toggle org-ol-tree.

- If the org-ol-tree window is visible hide it;

- If a org-ol-tree buffer exists, but is not visible show it.

- If no org-ol-tree buffer exists for the current Org-file buffer create and
  show it."
  (interactive)
  (unless (or org-ol-tree--buffer-p (buffer-live-p org-ol-tree--buffer) (eq major-mode 'org-mode))
    (user-error "Org Outline Tree can only be used with Org buffers"))
  (pcase (org-ol-tree-ui--visibility)
    ('visible
     (if org-ol-tree--buffer-p
         (delete-window (org-ol-tree-ui--get-window))
       (org-ol-tree-ui--setup-window nil)))
    ('exists
     (org-ol-tree-ui--setup-buffer)
     (org-ol-tree-ui--setup-window t)
     (org-ol-tree-action--refresh))
    ('none
     (org-ol-tree-action--init))))




(provide 'org-ol-tree)

;;; org-ol-tree.el ends here
