(defpackage #:slynk-package-inferred
  (:use :cl)
  (:import-from #:slynk)
  (:export #:disable
           #:enable))
(in-package #:slynk-package-inferred)


(defvar *original-hook-value*)


(defun is-from-same-system (left-package right-package)
  (let* ((left-package-name (package-name left-package))
         (right-package-name (package-name right-package))
         (slash-in-left (position #\/ left-package-name))
         (slash-in-right (position #\/ right-package-name)))
    (string= (subseq left-package-name
                     0 slash-in-left)
             (subseq right-package-name
                     0 slash-in-right))))


(defun sort-autocompletion-results (left right)
  (cond
    ;; We need this branch to keep ordering by
    ;; score between symbols from the same package
    ((is-from-same-system (symbol-package left)
                          (symbol-package right))
     nil)
    ;; This will move results from the current-buffer's
    ;; package to the front.
    ((is-from-same-system (symbol-package left)
                          slynk-completion:*current-package*)
     t)))


(defun enable ()
  (let ((var (find-symbol "*COMPLETION-SORT-PREDICATE*" :slynk-completion)))
    (cond
      (var
       (setf *original-hook-value*
             (symbol-value var))
       (setf (symbol-value var)
             'sort-autocompletion-results))
      (t
       (warn "This contrib can work only with SLY from https://github.com/svetlyak40wt/sly/tree/patches"))))
  (values))


(defun disable ()
  (let ((var (find-symbol "*COMPLETION-SORT-PREDICATE*" :slynk-completion)))
    (when var
      (setf (symbol-value var)
            *original-hook-value*))))


(provide 'slynk-package-inferred)
