#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.variable-environment
  (:use :cl
        :mini-lang.lang.symbol
        :mini-lang.lang.type)
  (:export :empty-variable-environment
           :extend-variable-environment
           :variable-environment-exists-p
           :query-variable-environment))
(in-package :mini-lang.lang.variable-environment)


;;
;; Variable environment

(defun empty-variable-environment ()
  nil)

(defun extend-variable-environment (var type venv)
  (check-type var mini-lang-symbol)
  (check-type type mini-lang-type)
  (let ((var1 (unique-var var type)))
    (values (acons var var1 venv) var1)))

(defun variable-environment-exists-p (var venv)
  (check-type var mini-lang-symbol)  
  (and (cdr (assoc var venv))
       t))

(defun query-variable-environment (var venv)
  (check-type var mini-lang-symbol)
  (or (cdr (assoc var venv))
      (error "The variable ~S not found." var)))

(defvar *genvar-counter* 0)

(defun genvar (var)
  (prog1 (intern (format nil "~A~A" var *genvar-counter*))
    (incf *genvar-counter*)))

(defun unique-var (var type)
  (cond
    ((scalar-type-p type) (genvar var))
    ((vector-type-p type) (loop repeat (vector-type-size type)
                             collect (genvar var)))
    ((array-type-p type) (genvar var))
    (t (error "Must not be reached."))))
