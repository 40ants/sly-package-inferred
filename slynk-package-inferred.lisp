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


(defun qualified-matching (pattern home-package)
  "Find package-qualified symbols flex-matching PATTERN.
  Return, as two values, a set of matches for external symbols,
  package-qualified using one colon, and another one for internal
  symbols, package-qualified using two colons.

  The matches in the two sets are not guaranteed to be in their final
  order, i.e. they are not sorted (except for the fact that
  qualifications with shorter package nicknames are tried first).

  Matches are produced by COLLECT-IF-MATCHES (which see)."
  (let* ((first-colon (position #\: pattern))
         (starts-with-colon (and first-colon (zerop first-colon)))
         (two-colons (and first-colon (< (1+ first-colon) (length pattern))
                          (eq #\: (aref pattern (1+ first-colon)))))
         (home-package-system (find-primary-system home-package)))
    
    (if (and starts-with-colon
             (not two-colons))
        (values nil nil)
        (let* ((package-local-nicknames
                 (slynk-backend:package-local-nicknames home-package))
               (nicknames-by-package
                 (let ((ret (make-hash-table)))
                   (loop for (short . full) in
                         package-local-nicknames
                         do (push short (gethash (find-package full)
                                                 ret)))
                   ret)))
          (slynk-completion::collecting (collect-same-system-external
                                         collect-external
                                         collect-same-system-internal
                                         collect-internal)
            (loop
              with use-list = (package-use-list home-package)
              for package in (remove +keyword-package+ (list-all-packages))
              for package-system = (find-primary-system package)
              for sorted-nicknames = (and (or first-colon
                                              (not (eq package home-package)))
                                          (sort (append
                                                 (gethash package nicknames-by-package)
                                                 (package-nicknames package)
                                                 (list (package-name package)))
                                                #'<
                                                :key #'length))
              for seen = (make-hash-table)
              when sorted-nicknames
                do (do-symbols (s package)
                     (unless (gethash s seen)
                       (setf (gethash s seen) t)
                       (let ((status (nth-value 1 (find-symbol (symbol-name s) package)))
                             (same-system (and home-package-system
                                               (equal home-package-system
                                                      package-system))))
                         (flet ((collect-using (func-to-collect symbol-name-template)
                                  (loop for nickname in sorted-nicknames
                                        do (slynk-completion::collect-if-matches
                                            func-to-collect
                                            pattern
                                            (format nil symbol-name-template
                                                    nickname
                                                    (symbol-name s))
                                            s))))
                           (cond ((and (eq status :external)
                                       (or first-colon
                                           (not (member (symbol-package s) use-list))))
                                  (collect-using (if same-system
                                                     #'collect-same-system-external
                                                     #'collect-external)
                                                 "~a:~a"))
                                 ((and two-colons
                                       (eq status :internal))
                                  (collect-using (if same-system
                                                     #'collect-same-system-internal
                                                     #'collect-internal)
                                                 "~a::~a")))))))))))))


(defun get-completions (pattern package &key (limit 300))
  (loop with (same-system-external external same-system-internal internal)
          = (multiple-value-list (qualified-matching pattern package))
        for e in (append (sort-by-score
                          (keywords-matching pattern))
                         (sort-by-score
                          same-system-external)
                         (sort-by-score
                          same-system-internal)
                         (sort-by-score
                          (append (accessible-matching pattern package)
                                  external))
                         (sort-by-score
                          internal))
        for i upto limit
        collect e))


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
