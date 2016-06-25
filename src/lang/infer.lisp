#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.infer
  (:use :cl
        :mini-lang.lang.type
        :mini-lang.lang.built-in
        :mini-lang.lang.syntax
        :mini-lang.lang.unification-environment
        :mini-lang.lang.type-environment)
  (:export :infer))
(in-package :mini-lang.lang.infer)


;;
;; Type inference

(defun infer (form tenv uenv)
  (cond
    ((literal-p form) (infer-literal form tenv uenv))
    ((reference-p form) (infer-reference form tenv uenv))
    ((the-p form) (infer-the form tenv uenv))
    ((if-p form) (infer-if form tenv uenv))
    ((let-p form) (infer-let form tenv uenv))
    ((application-p form) (infer-application form tenv uenv))
    (t (error "The value ~S is an invalid form." form))))

(defun infer-literal (form tenv uenv)
  (declare (ignore tenv))
  (cond
    ((int-literal-p form) (values 'int uenv))
    ((float-literal-p form) (values 'float uenv))
    ((double-literal-p form) (values 'double uenv))
    (t (error "Must not be reached."))))

(defun infer-reference (form tenv uenv)
  (let ((type (query-type-environment form tenv)))
    (if type
        (let ((type1 (query-unification-environment type uenv)))
          (values type1 uenv)))))

(defun infer-the (form tenv uenv)
  (let ((type (the-type form))
        (form1 (the-form form)))
    (multiple-value-bind (type1 uenv1) (infer form1 tenv uenv)
      (unify type type1 uenv1))))

(defun infer-if (form tenv uenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (multiple-value-bind (type1 uenv1) (infer test-form tenv uenv)
      (multiple-value-bind (_ uenv2) (unify type1 'bool uenv1)
        (declare (ignore _))
        (multiple-value-bind (type3 uenv3) (infer then-form tenv uenv2)
          (multiple-value-bind (type4 uenv4) (infer else-form tenv uenv3)
            (unify type3 type4 uenv4)))))))

(defun infer-let (form tenv uenv)
  (flet ((aux (ret binding)
           (destructuring-bind (tenv1 . uenv1) ret
             (destructuring-bind (var form) binding
               (multiple-value-bind (type uenv2) (infer form tenv uenv1)
                 (let ((tenv2 (extend-type-environment var type tenv1)))
                   (cons tenv2 uenv2)))))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (destructuring-bind (tenv1 . uenv1)
          (reduce #'aux bindings :initial-value (cons tenv uenv))
        (infer body tenv1 uenv1)))))

(defun infer-application (form tenv uenv)
  (flet ((aux (uenv argtype-operand)
           (destructuring-bind (argtype . operand) argtype-operand
             (multiple-value-bind (type1 uenv1) (infer operand tenv uenv)
               (multiple-value-bind (_ uenv2) (unify argtype type1 uenv1)
                 (declare (ignore _))
                 uenv2)))))
    (let ((operator (application-operator form))
          (operands (application-operands form)))
      (let ((argc (built-in-argc operator)))
        (unless (= argc (length operands))
          (error "Invalid number of arguments: ~S" (length operands))))
      (let* ((type-scheme (built-in-type-scheme operator))
             (type (type-scheme-to-type type-scheme)))
        (let ((argtypes (type-arg-types type))
              (return-type (type-return-type type)))
          (let* ((uenv1 (reduce #'aux (mapcar #'cons argtypes operands)
                                :initial-value uenv))
                 (type1 (query-unification-environment return-type uenv1)))
            (values type1 uenv1)))))))

(defun type-arg-types (type)
  (butlast type))

(defun type-return-type (type)
  (car (last type)))
