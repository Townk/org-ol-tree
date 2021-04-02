;;; buttercup-matchers.el --- Helper matchers for tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thiago Alves
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'buttercup)
(require 's)

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


;;; buttercup-matchers.el ends here
