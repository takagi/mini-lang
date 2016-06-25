#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.syntax
  (:use :cl
        :mini-lang.lang.symbol
        :mini-lang.lang.type)
  (:export ;; Literal
           :literal-p
           :int-literal-p
           :float-literal-p
           :double-literal-p
           ;; Reference
           :reference-p
           ;; The
           :the-p
           :the-type
           :the-form
           ;; IF
           :if-p
           :if-test-form
           :if-then-form
           :if-else-form
           ;; LET
           :let-p
           :let-bindings
           :let-body
           ;; Application
           :application-p
           :application-operator
           :application-operands))
(in-package :mini-lang.lang.syntax)


;;
;; Literal

(defun literal-p (form)
  (or (int-literal-p form)
      (float-literal-p form)
      (double-literal-p form)))

(defun int-literal-p (form)
  (integerp form))

(defun float-literal-p (form)
  (typep form 'single-float))

(defun double-literal-p (form)
  (typep form 'double-float))


;;
;; Reference

(defun reference-p (form)
  (mini-lang-symbol-p form))


;;
;; The

(defun the-p (form)
  (cl-pattern:match form
    (('the _ _) t)
    (('the . _) (error "The form ~S is malformed." form))
    (_ nil)))

(defun the-type (form)
  (assert (the-p form))
  (cl-pattern:match form
    (('the type _) (parse-type type))))

(defun the-form (form)
  (assert (the-p form))
  (cl-pattern:match form
    (('the _ form1) form1)))


;;
;; IF

(defun if-p (form)
  (cl-pattern:match form
    (('if _ _ _) t)
    (('if . _) (error "The form ~S is malformed." form))
    (_ nil)))

(defun if-test-form (form)
  (assert (if-p form))
  (cl-pattern:match form
    (('if test-form _ _) test-form)))

(defun if-then-form (form)
  (assert (if-p form))
  (cl-pattern:match form
    (('if _ then-form _) then-form)))

(defun if-else-form (form)
  (assert (if-p form))
  (cl-pattern:match form
    (('if _ _ else-form) else-form)))


;;
;; LET

(defun binding-p (object)
  (cl-pattern:match object
    ((var _) (mini-lang-symbol-p var))
    (_ nil)))

(defun let-p (form)
  (cl-pattern:match form
    (('let bindings _) (or (every #'binding-p bindings)
                           (error "The form ~S is malformed." form)))
    (('let . _) (error "The form ~S is malformed." form))
    (_ nil)))

(defun let-bindings (form)
  (assert (let-p form))
  (cl-pattern:match form
    (('let bindings _) bindings)))

(defun let-body (form)
  (assert (let-p form))
  (cl-pattern:match form
    (('let _ body) body)))


;;
;; Application

(defun application-p (form)
  (consp form))

(defun application-operator (form)
  (assert (application-p form))
  (car form))

(defun application-operands (form)
  (assert (application-p form))
  (cdr form))
