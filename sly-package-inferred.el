;;; sly-package-inferred.el --- Package inferred systems support for SLY  -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/40ants/sly-package-inferred
;; Keywords: languages, lisp, sly
;; Package-Requires: ((sly "1.0.0-beta2"))
;; Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
;; 
;; Copyright (C) 2018 Alexander Artemenko
;;
;;; Code:

(require 'sly)

(define-sly-contrib sly-package-inferred
  "Define the `sly-package-inferred' contrib.
Depends on the `slynk-quicklisps' ASDF system Insinuates itself
in `sly-editing-mode-hook', i.e. lisp files."
  (:slynk-dependencies slynk-package-inferred)
  (:on-load (add-hook 'sly-mode-hook 'sly-package-inferred-enable))
  (:on-unload (remove-hook 'sly-mode-hook 'sly-package-inferred-disable)))


(defun sly-package-inferred-enable ()
  (sly-eval-async `(slynk-package-inferred:enable)
                  (lambda (retval)
                    nil)))


(defun sly-package-inferred-disable ()
  (sly-eval-async `(slynk-package-inferred:disable)
                  (lambda (retval)
                    nil)))


;;; Automatically add ourselves to `sly-contribs' when this file is loaded
;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-package-inferred 'append))

(provide 'sly-package-inferred)
