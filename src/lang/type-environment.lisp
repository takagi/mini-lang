#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.type-environment
  (:use :cl
        :mini-lang.lang.symbol
        :mini-lang.lang.type
        :mini-lang.lang.unification-environment)
  (:export :empty-type-environment
           :extend-type-environment
           :query-type-environment
           :substitute-type-environment))
(in-package :mini-lang.lang.type-environment)


;;
;; Type environment

(defun empty-type-environment ()
  nil)

(defun extend-type-environment (var type tenv)
  (check-type var mini-lang-symbol)
  (check-type type mini-lang-type)
  (acons var type tenv))

(defun query-type-environment (var tenv)
  (check-type var symbol)
  (or (cdr (assoc var tenv))
      (error "The variable ~S not found." var)))

(defun substitute-type-environment (uenv tenv)
  (loop for (var . type) in tenv
     collect
       (let ((type1 (query-unification-environment type uenv)))
         (cons var type1))))
