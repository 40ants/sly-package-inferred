(defpackage #:slynk-package-inferred
  (:use :cl #:slynk-api)
  (:import-from #:slynk-completion
                #:sort-by-score
                #:keywords-matching
                #:accessible-matching)
  (:export
   #:disable
   #:enable))
(in-package #:slynk-package-inferred)


(defparameter *package-systems-cache* (make-hash-table :test 'eq))


(defvar *original-get-completions* nil)


(defun find-primary-system (package)
  "Searches a primary asdf system for the package.
   For example, for package ultralisp/models/user it will return \"ultralisp\" system.
   Used to give higher score to symbols from the same system dyring autocompletion."
  
  (check-type package package)
  (let ((cached-value (gethash package *package-systems-cache* :absent)))
    (if (eql cached-value :absent)
        (setf (gethash package *package-systems-cache*)
              (handler-case (asdf:primary-system-name
                             (asdf:find-system
                              (make-symbol (package-name package))))
                (asdf/find-component:missing-component ()
                  nil)))
        (values cached-value))))


(defun get-completions (pattern package &key (limit 300))
  "This variant of get-completions searches for symbols
   which are from the same primary system as current package
   and moves them to the from of the list."
  
  (let ((results (slynk-completion:get-completions pattern package :limit limit))
        (current-primary-system (find-primary-system package)))
    (loop for item in results
          for symbol = (second item)
          for symbol-package = (when current-primary-system
                                 (symbol-package symbol))
          for symbol-primary-system = (when symbol-package
                                        (find-primary-system symbol-package))
          for from-the-same-system = (when symbol-primary-system
                                       (string-equal current-primary-system
                                                     symbol-primary-system))
          when from-the-same-system
            collect item into same-system-items
          unless from-the-same-system
            collect item into other-items
          finally
             (return (append same-system-items
                             other-items)))))


(defun enable ()
  (unless (eql slynk-completion:*get-completions*
               'get-completions)
    (setf *original-get-completions*
          slynk-completion:*get-completions*))
  
  (setf slynk-completion:*get-completions*
        'get-completions))


(defun disable ()
  (when *original-get-completions*
    (setf slynk-completion:*get-completions*
          *original-get-completions*)
    (setf *original-get-completions*
          nil)))


(provide 'slynk-package-inferred)
