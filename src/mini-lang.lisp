#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang
  (:use :cl
        :cl-pattern
        :lol)
  (:export :my-compile
           :scalar
           :vector))
(in-package :mini-lang)


;;; compile

(defmacro my-compile (exp)
  (compile-exp exp (empty-type-environment)))

(defun compile-exp (exp type-env)
  (cond ((scalar-literal-p exp) exp)
        ((vector-literal-p exp) (compile-vector-literal exp))
        ((let-p exp) (compile-let exp type-env))
        ((variable-p exp) (compile-variable exp type-env))
        ((application-p exp) (compile-application exp type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))


;;; scalar literal

(defun scalar-literal-p (exp)
  (typep exp 'double-float))


;;; vector literal

(defun vector-literal-p (exp)
  (match exp
    ((x y z) (every #'scalar-literal-p (list x y z)))
    (_       nil)))

(defun compile-vector-literal (exp)
  (match exp
    ((x y z) `(values ,x ,y ,z))))


;;; let expression

;;
;; (let ((x 1d0)
;;       (y (1d0 1d0 1d0)))
;;   ...)
;; =>
;; (let ((x 1d0))
;;   (multiple-value-bind (y0 y1 y2) (values 1d0 1d0 1d0))
;;     ...))
;;

(defun let-p (exp)
  (match exp
    (('let . _) t)
    (_ nil)))

(defun compile-let (exp type-env)
  (compile-let% (let-binds exp) (let-exp exp) type-env))

(defun let-binds (exp)
  (match exp
    (('let binds _) binds)))

(defun let-exp (exp)
  (match exp
    (('let _ exp2) exp2)))

(defun compile-let% (binds exp type-env)
  (if (null binds)
      (compile-exp exp type-env)
      (match (car binds)
        ((_ 'scalar _) (compile-scalar-bind binds exp type-env))
        ((_ 'vector _) (compile-vector-bind binds exp type-env)))))

(defun compile-scalar-bind (binds exp type-env)
  (match binds
    (((var 'scalar val) . rest)
      (let ((type-env2 (add-type-environment var 'scalar type-env)))
        `(let ((,var ,(compile-exp val type-env)))
           ,(compile-let% rest exp type-env2))))))

(defun compile-vector-bind (binds exp type-env)
  (match binds
    (((var 'vector val) . rest)
      (let ((type-env2 (add-type-environment var 'vector type-env)))
        (multiple-value-bind (x y z) (make-symbols-for-values var)
          `(multiple-value-bind (,x ,y ,z) ,(compile-exp val type-env)
             ,(compile-let% rest exp type-env2)))))))


;;; variable

;;
;; when type of exp is scalar:
;;   (let ((x 1d0))
;;     x)
;; when thpe of exp is vector:
;;   (multiple-value-bind (x0 x1 x2) (values 1d0 1d0 1d0)
;;     (values x0 x1 x2))
;;

(defun variable-p (exp)
  (symbolp exp))

(defun compile-variable (var type-env)
  (cond ((scalar-type-p var type-env) var)
        ((vector-type-p var type-env) (multiple-value-bind (x y z)
                                          (make-symbols-for-values var)
                                        `(values ,x ,y ,z)))))

(defun make-symbols-for-values (s)
  (values (symb s 0) (symb s 1) (symb s 2)))


;;; function application

(defconstant built-in-functions% '(+ (((scalar scalar) scalar +)
                                      ((vector vector) vector vector-+))
                                   - (((scalar scalar) scalar -)
                                      ((vector vector) vector vector--))
                                   * (((scalar scalar) scalar *)
                                      ((vector scalar) vector vector-*.)
                                      ((scalar vector) vector vector-.*))
                                   / (((scalar scalar) scalar /)
                                      ((vector scalar) vector vector-/.))))

(defun application-p (exp)
  (match exp
    ((op . _) (not (null (find op built-in-functions))))
    (_ nil)))

(defun compile-application (exp type-env)
  (match exp
    ((op . operands) (let ((candidates (operation-candidates op)))
                       (aif (infer-op operands candidates type-env)
                            `(,it ,@(mapcar (lambda (exp)
                                              (compile-exp exp type-env))
                                            operands))
                            (error (format nil
                                           "invalid application: ~A" exp)))))))

(defun infer-op (operands candidates type-env)
  (aif (infer-function operands candidates type-env)
       (caddr it)))

(defun infer-return-type (operands candidates type-env)
  (aif (infer-function operands candidates type-env)
       (cadr it)))

(defun infer-function (operands candidates type-env)
  (find (operand-types operands type-env)
        candidates
        :key #'car :test #'equal))

;; e.g.
;; (scalar scalar)
(defun operand-types (operands type-env)
  (mapcar (lambda (exp)
            (type-of-exp exp type-env)) operands))

;; e.g.
;; (((scalar scalar) scalar +)
;;  ((vector vector) vector vector-+))
(defun operation-candidates (op)
  (getf built-in-functions% op))


;;; type

(defun type-of-exp (exp type-env)
  (cond ((scalar-literal-p exp) 'scalar)
        ((vector-literal-p exp) 'vector)
        ((let-p exp) (type-of-let exp type-env))
        ((variable-p exp) (type-of-variable exp type-env))
        ((application-p exp) (type-of-application exp type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))

(defun scalar-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'scalar))

(defun vector-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'vector))

(defun type-of-let (exp type-env)
  (type-of-let% (let-binds exp) (let-exp exp) type-env))

(defun type-of-let% (binds exp type-env)
  (if (null binds)
      (type-of-exp exp type-env)
      (match (car binds)
        ((_ 'scalar _) (type-of-scalar-bind binds exp type-env))
        ((_ 'vector _) (type-of-vector-bind binds exp type-env)))))

(defun type-of-scalar-bind (binds exp type-env)
  (match binds
    (((var 'scalar _) . rest)
      (let ((type-env2 (add-type-environment var 'scalar type-env)))
        (type-of-let% rest exp type-env2)))))

(defun type-of-vector-bind (binds exp type-env)
  (match binds
    (((var 'vector _) . rest)
      (let ((type-env2 (add-type-environment var 'vector type-env)))
        (type-of-let% rest exp type-env2)))))

(defun type-of-variable (var type-env)
  (match (lookup-type-environment var type-env)
    (nil  (error (format nil "unbound variable: ~A" var)))
    (type type)))

(defun type-of-application (exp type-env)
  (match exp
    ((op . operands) (let ((candidates (operation-candidates op)))
                       (aif (infer-return-type operands candidates type-env)
                            it
                            (error (format nil
                                           "invalid application: ~A" exp)))))))


;;; type environment

;; env   ::= (<variable> . <type>)*

(defun empty-type-environment ()
  '())

(defun add-type-environment (var type type-env)
  (assert-type type)
  (cons (cons var type) type-env))

(defun lookup-type-environment (var type-env)
  (match (assoc var type-env)
    ((_ . type) type)
    (_          nil)))

(defmacro assert-type (type)
  `(assert (or (eq ,type 'scalar)
               (eq ,type 'vector))))


;;; utilities

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))
