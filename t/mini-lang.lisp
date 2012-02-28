#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-test
  (:use :cl
        :mini-lang
        :cl-test-more))
(in-package :mini-lang-test)

(plan nil)

(defvar *empty-var-env* (mini-lang::empty-variable-environment))
(defvar *empty-type-env* (mini-lang::empty-type-environment))


;;; test vec3

(defun is-vec3 (got expected &rest args)
  (apply #'is `(,got ,expected ,@args :test ,#'vec3=)))

(is-vec3 (make-vec3 1d0 1d0 1d0) (make-vec3 1d0 1d0 1d0)
         "make-vec3 and vec3=")

(is-vec3 (zero-vec3) (make-vec3 0d0 0d0 0d0)
         "zero-vec3")

(is-vec3 (vec3-negate (make-vec3 1d0 1d0 1d0))
         (make-vec3 -1d0 -1d0 -1d0) "vec3-negate")

(is-vec3 (vec3-+ (make-vec3 1d0 1d0 1d0) (make-vec3 2d0 2d0 2d0))
         (make-vec3 3d0 3d0 3d0) "vec3-+")

(is-vec3 (vec3-- (make-vec3 1d0 1d0 1d0) (make-vec3 1d0 1d0 1d0))
         (make-vec3 0d0 0d0 0d0) "vec3--")


;;; test operation interfaces

(is-expand (setf-scalar x 1d0) (setf x (mini-lang::compile-mini-lang 1d0))
           "setf-scalar")
(is-expand (setf-scalar-array x i 1d0)
           (setf (scalar-aref x i) (mini-lang::compile-mini-lang 1d0))
           "setf-scalar-array")

(is-expand (incf-scalar x 1d0) (setf-scalar x (+ x 1d0)) "incf-scalar")
(is-expand (incf-scalar-array x i 1d0)
           (setf-scalar-array x i (+ (scalar-aref x i) 1d0))
           "incf-scalar-array")

(is-expand (setf-vec3 x (1d0 1d0 1d0))
           (setf (mini-lang::vec3* x)
                 (mini-lang::compile-mini-lang (1d0 1d0 1d0)))
           "setf-vec3")
(is-expand (setf-vec3-array x i (1d0 1d0 1d0))
           (setf (mini-lang::vec3-aref* x i)
                 (mini-lang::compile-mini-lang (1d0 1d0 1d0)))
           "setf-vec3-array")

(is-expand (incf-vec3 x (1d0 1d0 1d0)) (setf-vec3 x (+ x (1d0 1d0 1d0)))
           "incf-vec3")
(is-expand (incf-vec3-array x i (1d0 1d0 1d0))
           (setf-vec3-array x i (+ (1d0 1d0 1d0) (vec3-aref x i)))
           "incf-vec3-array")

(is-expand (for-scalar-array x i
             (setf-scalar-array x i 1d0))
           (dotimes (i (scalar-array-size x))
             (setf-scalar-array x i 1d0))
           "for-scalar-array and setf-scalar-array")

(is-expand (for-vec3-array x i
             (setf-vec3-array x i (1d0 1d0 1d0)))
           (dotimes (i (vec3-array-size x))
             (setf-vec3-array x i (1d0 1d0 1d0)))
           "for-vec3-array and setf-vec3-array")

(is (let ((x (make-scalar-array 1)))
      (for-scalar-array x i
        (setf-scalar-array x i 1d0)
        (incf-scalar-array x i 1d0))
      (scalar-aref x 0))
    2d0 "incf-scalar-array")

(is (let ((x (make-vec3-array 1)))
      (for-vec3-array x i
        (setf-vec3-array x i (1d0 1d0 1d0))
        (incf-vec3-array x i (1d0 1d0 1d0)))
      (mini-lang::vec3-aref* x 0))
    (mini-lang::vec3-values* 2d0 2d0 2d0) "incf-vec3-array")


;;; test compile-exp

(is (mini-lang::compile-exp 1d0 *empty-type-env*)
    1d0 "compile-exp 1")
(is (mini-lang::compile-exp '(1d0 1d0 1d0) *empty-type-env*)
    '(mini-lang::vec3-values* 1d0 1d0 1d0)
    "compile-exp 2")


;;; test literal

(is (mini-lang::bool-literal-p 't) t "bool-literal-p 1")
(is (mini-lang::bool-literal-p 'nil) t "bool-literal-p 2")

(is (mini-lang::int-literal-p 1) t "int-literal-p")

(is (mini-lang::scalar-literal-p '1d0) t "scalar-literal-p")

(is (mini-lang::vec3-literal-p '(1d0 1d0 1d0)) t "vec3-literal-p")
(is (mini-lang::compile-vec3-literal '(1d0 1d0 1d0))
    '(mini-lang::vec3-values* 1d0 1d0 1d0)
    "compile-vec3-literal")


;;; test external environment reference

(is (mini-lang::external-environment-reference-p '(bool x)) t
    "external-environment-reference-p 1")
(is (mini-lang::external-environment-reference-p '(int x)) t
    "external-environment-reference-p 2")
(is (mini-lang::external-environment-reference-p '(scalar x)) t
    "external-environment-reference-p 3")
(is (mini-lang::external-environment-reference-p '(scalar x y)) nil
    "external-environment-reference-p 4")
(is (mini-lang::external-environment-reference-p '(vec3 x)) t
    "external-environment-reference-p 5")
(is (mini-lang::external-environment-reference-p '(vec3 x y z)) t
    "external-environment-reference-p 6")
(is (mini-lang::external-environment-reference-p '(scalar-aref x i)) t
    "external-environment-reference-p 7")
(is (mini-lang::external-environment-reference-p '(vec3-aref x i)) t
    "external-environment-reference-p 8")

(is (mini-lang::compile-external-environment-reference '(bool x))
    'x "compile-external-environment-reference 1")
(is (mini-lang::compile-external-environment-reference '(int x))
    `(the int x) "compile-external-environment-reference 2")
(is (mini-lang::compile-external-environment-reference '(scalar x))
    '(the scalar x) "compile-external-environment-reference 3")
(is (mini-lang::compile-external-environment-reference '(vec3 x))
    '(mini-lang::vec3* x) "compile-external-environment-reference 4")
(is (mini-lang::compile-external-environment-reference '(vec3 x y z))
    '(mini-lang::vec3-values* x y z) "compile-external-environment-reference 5")
(is (mini-lang::compile-external-environment-reference '(scalar-aref x i))
    '(scalar-aref x i) "compile-external-environment-reference 6")
(is (mini-lang::compile-external-environment-reference '(vec3-aref x i))
    '(mini-lang::vec3-aref* x i) "compile-external-environment-reference 7")


;;; test let expression

(is (mini-lang::let-p '(let)) t "let-p")
(is (mini-lang::let-binds '(let ((x scalar 1d0)) x)) '((x scalar 1d0))
    "let-binds")
(is (mini-lang::let-exp '(let ((x scalar 1d0)) x)) 'x "let-exp")

(is (mini-lang::compile-let% '((x bool t)) '1d0 *empty-type-env*)
    '(let ((x t)) 1d0) "compile-let% 1")
(is (mini-lang::compile-let% '((x int 1)) 1 *empty-type-env*)
    `(let ((x 1)) 1) "compile-let% 2")
(is (mini-lang::compile-let% '((x scalar 1d0)) '1d0 *empty-type-env*)
    '(let ((x 1d0)) 1d0) "compile-let% 3")
(is (mini-lang::compile-let% '((x vec3 (1d0 1d0 1d0))) '1d0 *empty-type-env*)
    '(multiple-value-bind (x-0 x-1 x-2) (mini-lang::vec3-values* 1d0 1d0 1d0)
       1d0) "compile-let% 4")

(is-error (mini-lang::compile-let% '((x bool 1d0)) 'x *empty-type-env*)
          simple-error "compile-let% 5")
(is-error (mini-lang::compile-let% '((x int 1d0)) 'x *empty-type-env*)
          simple-error "compile-let% 6")
(is-error (mini-lang::compile-let% '((x scalar t)) 'x *empty-type-env*)
          simple-error "compile-let% 7")
(is-error (mini-lang::compile-let% '((x vec3 1d0)) 'x *empty-type-env*)
          simple-error "compile-let% 8")

(is (mini-lang::compile-let% '((x bool t)) 'x *empty-type-env*)
    '(let ((x t)) x) "compile-let% 9")
(is (mini-lang::compile-let% '((x int 1)) 'x *empty-type-env*)
    '(let ((x 1)) x) "compile-let% 10")
(is (mini-lang::compile-let% '((x scalar 1d0)) 'x *empty-type-env*)
    '(let ((x 1d0)) x) "compile-let% 11")
(is (mini-lang::compile-let% '((x vec3 (1d0 1d0 1d0))) 'x *empty-type-env*)
    '(multiple-value-bind (x-0 x-1 x-2) (mini-lang::vec3-values* 1d0 1d0 1d0)
       (mini-lang::vec3-values* x-0 x-1 x-2)) "compile-let% 12")

(is (mini-lang::compile-let '(let ((x bool t)) x) *empty-type-env*)
    '(let ((x t)) x) "compile-let 1")
(is (mini-lang::compile-let '(let ((x int 1)) x) *empty-type-env*)
    '(let ((x 1)) x) "compile-let 2")
(is (mini-lang::compile-let '(let ((x scalar 1d0)) x) *empty-type-env*)
    '(let ((x 1d0)) x) "compile-let 3")
(is (mini-lang::compile-let '(let ((x vec3 (1d0 1d0 1d0))) x) *empty-type-env*)
    '(multiple-value-bind (x-0 x-1 x-2) (mini-lang::vec3-values* 1d0 1d0 1d0)
       (mini-lang::vec3-values* x-0 x-1 x-2)) "compile-let 4")
(is (mini-lang::compile-let '(let ((y scalar 1d0))
                               (let ((x vec3 (1d0 1d0 1d0)))
                                 y))
                            *empty-type-env*)
    '(let ((y 1d0))
       (multiple-value-bind (x-0 x-1 x-2) (mini-lang::vec3-values* 1d0 1d0 1d0)
         y))
    "compile-let 5")


;;; test if expression

(is (mini-lang::if-p '(if t 2d0 1d0)) t "if-p")

(is (mini-lang::compile-if '(if t 2d0 1d0) *empty-type-env*)
    `(if t 2d0 1d0) "compile-if")


;;; test variable

(is (mini-lang::variable-p 'x) t "variable-p")

(let ((type-env (mini-lang::add-type-environment 'x 'bool *empty-type-env*)))
  (is (mini-lang::compile-variable 'x type-env) 'x "compile-variable 1"))

(let ((type-env (mini-lang::add-type-environment 'x 'int *empty-type-env*)))
  (is (mini-lang::compile-variable 'x type-env) 'x "compile-variable 2"))

(let ((type-env (mini-lang::add-type-environment 'x 'scalar *empty-type-env*)))
  (is (mini-lang::compile-variable 'x type-env) 'x "compile-variable 3"))

(let ((type-env (mini-lang::add-type-environment 'x 'vec3 *empty-type-env*)))
  (is (mini-lang::compile-variable 'x type-env)
      '(mini-lang::vec3-values* x-0 x-1 x-2) "compile-variable 4"))

(is (mini-lang::make-symbols-for-values 'x)
    '(x-0 x-1 x-2) "make-symbols-for-values")


;;; test user-defined function

(mini-lang::reset-unique-variables-counter)
(is (mini-lang::make-unique-variable 'x) 'x1 "make-unique-variable")

(is (mini-lang::make-user-defined-function-args '(x y) '(scalar vec3))
    '((x scalar) (y vec3)) "make-user-defined-function-args")

(mini-lang::reset-unique-variables-counter)
(define-function f ((scalar x))
  (+ x 1d0 2d0))
(is (mini-lang::user-defined-function-name 'f)
    'f "user-defined-function-name 1")
(is (mini-lang::user-defined-function-args 'f)
    '((x1 scalar)) "user-defined-function-args 1")
(is (mini-lang::user-defined-function-return-type 'f)
    'scalar "user-defined-function-return-type 1")
(is (mini-lang::user-defined-function-compiled-expression 'f)
    '(+ (+ x1 1d0) 2d0) "user-defined-function-compiled-expression 1")

(mini-lang::reset-unique-variables-counter)
(define-function g ((scalar x) (vec3 y))
  (* x y))
(is (mini-lang::user-defined-function-name 'g)
    'g "user-defined-function-name 2")
(is (mini-lang::user-defined-function-args 'g)
    '((x1 scalar) (y2 vec3)) "user-defined-function-args 2")
(is (mini-lang::user-defined-function-return-type 'g)
    'vec3 "user-defined-function-return-type 2")
(is (mini-lang::user-defined-function-compiled-expression 'g)
    '(mini-lang::vec3-scale%* x1 (mini-lang::vec3-values* y2-0 y2-1 y2-2))
    "user-defined-funciton-compiled-expression 2")

(is-error (mini-lang::user-defined-function-name 'undefined-function)
          simple-error "user-defined-function-name 3")
(is-error (mini-lang::user-defined-function-args 'user-defined-funciton)
          simple-error "user-defined-function-args 3")
(is-error (mini-lang::user-defined-function-return-type 'undefined-function)
          simple-error "user-defined-function-return-type 3")
(is-error (mini-lang::user-defined-function-compiled-expression 'undefined-function)
          simple-error "user-defined-function-compiled-expression 3")

(let ((arg (car (mini-lang::user-defined-function-args 'f))))
  (is (mini-lang::user-defined-function-arg-var arg)
      'x1 "user-defined-function-arg-var")
  (is (mini-lang::user-defined-function-arg-type arg)
      'scalar "user-defined-function-arg-type"))


;;; test define-function and defined function's application

(clear-functions)

(mini-lang::reset-unique-variables-counter)
(define-function f ((scalar x))
  (+ x 1d0))
(is-error (define-function f x (+ x 1d0)) simple-error
          "define-function 1")
(is-error (define-function f (scalar x) (+ x 1d0)) simple-error
          "define-function 2")
(is (mini-lang::user-defined-application-p '(f 1d0)) t
    "user-defined-application-p 1")
(is (mini-lang::user-defined-application-p '(f2 1d0)) nil
    "user-defined-aplication-p 2")
(is (mini-lang::compile-user-defined-application '(f 1d0) *empty-type-env*)
    '(let ((x1 1d0))
       (+ x1 1d0)) "compile-user-defined-application 1")
(is-error (mini-lang::compile-user-defined-application '(f 1d0 1d0)
                                                       *empty-type-env*)
          simple-error "compile-user-defined-application 2")

(mini-lang::reset-unique-variables-counter)
(define-function g ((scalar x) (scalar y))
  (+ x y 1d0))
(is (mini-lang::user-defined-application-p '(g 1d0 1d0)) t
    "user-defined-application-p 3")
(is (mini-lang::compile-user-defined-application '(g 1d0 1d0) *empty-type-env*)
    '(let ((x1 1d0))
      (let ((y2 1d0))
        (+ (+ x1 y2) 1d0))) "compile-user-defined-application 3")

(mini-lang::reset-unique-variables-counter)
(define-function f ((vec3 x))  ; test redefinition of f
  (+ x (1d0 1d0 1d0)))
(is (mini-lang::user-defined-application-p '(f (1d0 1d0 1d0))) t
    "user-defined-application-p 4")
(is (mini-lang::compile-user-defined-application '(f (1d0 1d0 1d0))
                                                 *empty-type-env*)
    '(multiple-value-bind (x1-0 x1-1 x1-2) (mini-lang::vec3-values* 1d0 1d0 1d0)
       (mini-lang::vec3-add* (mini-lang::vec3-values* x1-0 x1-1 x1-2)
                             (mini-lang::vec3-values* 1d0 1d0 1d0)))
    "compile-user-defined-application 4")
(is-error (mini-lang::compile-user-defined-application '(f 1d0)
                                                       *empty-type-env*)
          simple-error "compile-user-defined-application 5")

(is-error (define-function f (x) x)
          simple-error "define-function 1")
(is-error (define-function f ((x)) x)
          simple-error "define-function 2")
(is-error (define-function f ((x x)) x)
          simple-error "define-function 3")
(is-error (define-function f ((scalar x 1d0)) x)
          simple-error "define-function 4")
(is-error (define-function f () (+ x (1d0 1d0 1d0)))
          simple-error "define-function 5")

(mini-lang::reset-unique-variables-counter)
(define-function f ((scalar x))
  x)
(is (mini-lang::compile-exp
     (mini-lang::single-assignment '(let ((x scalar 1d0))
                                     (+ x (f x)))
                                   *empty-var-env* *empty-type-env*)
     *empty-type-env*)
    '(let ((x2 1.0d0))
      (+ x2 (let ((x1 x2))
              x1)))
    "lexical scoping 1")

(mini-lang::reset-unique-variables-counter)
(define-function f ((scalar x))
  (let ((x scalar x))
    x))
(is (mini-lang::compile-exp
     (mini-lang::single-assignment '(let ((x scalar 1d0))
                                     (+ x (f x)))
                                   *empty-var-env* *empty-type-env*)
     *empty-type-env*)
    '(let ((x3 1d0))
      (+ x3 (let ((x1 x3))
              (let ((x2 x1))
                x2))))
    "lexical scoping 2")


;;; test application of built-in functions

(is (mini-lang::built-in-application-p '(+ 1d0 1d0)) t
    "built-in-application-p 1")
(is (mini-lang::built-in-application-p '(++ 1d0 1d0)) nil
    "build-in-application-p 2")

(is (mini-lang::compile-built-in-application '(+ 1d0 1d0) *empty-type-env*)
    '(+ 1d0 1d0) "compile-built-in-application 1")
(is (let ((type-env (mini-lang::make-type-environment '(x y) '(scalar scalar))))
      (mini-lang::compile-built-in-application '(+ x y) type-env))
    '(+ x y) "compile-built-in-application 2")
(is (mini-lang::compile-built-in-application '(+ (1d0 1d0 1d0) (1d0 1d0 1d0))
                                             *empty-type-env*)
    '(mini-lang::vec3-add* (mini-lang::vec3-values* 1d0 1d0 1d0)
                             (mini-lang::vec3-values* 1d0 1d0 1d0))
    "compile-built-in-application 3")
(is (let ((type-env (mini-lang::make-type-environment '(x y) '(vec3 vec3))))
      (mini-lang::compile-built-in-application '(+ x y) type-env))
    '(mini-lang::vec3-add* (mini-lang::vec3-values* x-0 x-1 x-2)
                             (mini-lang::vec3-values* y-0 y-1 y-2))
    "compile-built-in-application 4")

(is-error (mini-lang::compile-built-in-application '(++ 1d0 1d0)
                                                   *empty-type-env*)
          simple-error "compile-built-in-application 5")
(is-error (mini-lang::compile-built-in-application '(+ (1d0 1d0 1d0) 1d0)
                                                   *empty-type-env*)
          simple-error "compile-built-in-application 6")

(is (mini-lang::compile-built-in-application '(* (1d0 1d0 1d0) 1d0)
                                             *empty-type-env*)
    '(mini-lang::vec3-scale* (mini-lang::vec3-values* 1d0 1d0 1d0) 1d0)
    "compile-built-in-application 7")
(is (mini-lang::compile-built-in-application '(* 1d0 (1d0 1d0 1d0))
                                             *empty-type-env*)
    '(mini-lang::vec3-scale%* 1d0 (mini-lang::vec3-values* 1d0 1d0 1d0))
    "compile-built-in-application 8")

(is (mini-lang::operand-types '(x y) '((x . scalar) (y . scalar)))
    '(scalar scalar) "operand-types")
(is (mini-lang::infer-op '(x y)
                         (mini-lang::operation-candidates '+)
                         '((x . vec3) (y . vec3)))
    'mini-lang::vec3-add* "infer-op")
(is (mini-lang::infer-return-type '(x y)
                                  (mini-lang::operation-candidates '+)
                                  '((x . vec3) (y . vec3)))
    'vec3 "infer-return-type")

(is (mini-lang::compile-built-in-application '(norm (1d0 1d0 1d0))
                                             *empty-type-env*)
    '(mini-lang::vec3-norm* (mini-lang::vec3-values* 1d0 1d0 1d0))
    "compile-built-in-application 9")

(is (mini-lang::compile-built-in-application '(- 1d0) *empty-type-env*)
    '(- 1d0) "compile-built-in-application 10")
(is (mini-lang::compile-built-in-application '(- (1d0 1d0 1d0))
                                             *empty-type-env*)
    `(mini-lang::vec3-negate* (mini-lang::vec3-values* 1d0 1d0 1d0))
    "compile-built-in-application 11")

(is (mini-lang::compile-built-in-application '(exp 1d0) *empty-type-env*)
    '(exp 1d0) "compile-built-in-application 12")

(is (mini-lang::compile-built-in-application '(= 1 1) *empty-type-env*)
    '(= 1 1) "compile-built-in-application 13")


;;; test type

(is (mini-lang::type-of-exp t *empty-type-env*) 'bool "type-of-exp 1")
(is (mini-lang::type-of-exp nil *empty-type-env*) 'bool "type-of-exp 2")
(is (mini-lang::type-of-exp 1 *empty-type-env*) 'int "type-of-exp 3")
(is (mini-lang::type-of-exp 1d0 *empty-type-env*)
    'scalar "type-of-exp 4")
(is (mini-lang::type-of-exp '(1d0 1d0 1d0) *empty-type-env*)
    'vec3 "type-of-exp 5")

(is (let ((type-env (mini-lang::add-type-environment 'x 'scalar
                                                     *empty-type-env*)))
      (mini-lang::type-of-exp 'x type-env)) 'scalar "type-of-exp 6")

(is (mini-lang::type-of-external-environment-reference '(bool x)) 'bool
    "type-of-external-environment-reference 1")
(is (mini-lang::type-of-external-environment-reference '(int x)) 'int
    "type-of-external-environment-reference 2")
(is (mini-lang::type-of-external-environment-reference '(scalar x)) 'scalar
    "type-of-external-environment-reference 3")
(is (mini-lang::type-of-external-environment-reference '(vec3 x)) 'vec3
    "type-of-external-environment-reference 4")
(is (mini-lang::type-of-external-environment-reference '(vec3 x y z)) 'vec3
    "type-of-external-environment-reference 5")
(is (mini-lang::type-of-external-environment-reference '(scalar-aref x i))
    'scalar "type-of-external-environment-reference 6")
(is (mini-lang::type-of-external-environment-reference '(vec3-aref x i))
    'vec3 "type-of-external-environment-reference 7")

(is (mini-lang::type-of-let '(let ((x bool t)) x) *empty-type-env*)
    'bool "type-of-let 1")
(is (mini-lang::type-of-let '(let ((x int 1)) x) *empty-type-env*)
    'int "type-of-let 2")
(is (mini-lang::type-of-let '(let ((x scalar 1d0)) x) *empty-type-env*)
    'scalar "type-of-let 3")
(is (let ((type-env (mini-lang::add-type-environment 'y 'vec3
                                                     *empty-type-env*)))
      (mini-lang::type-of-let '(let ((x scalar 1d0)) y) type-env))
    'vec3 "type-of-let 4")

(is-error (mini-lang::type-of-let '(let ((x bool 1d0)) x) *empty-type-env*)
          simple-error "type-of-let 5")
(is-error (mini-lang::type-of-let '(let ((x int 1d0)) x) *empty-type-env*)
          simple-error "type-of-let 6")
(is-error (mini-lang::type-of-let '(let ((x scalar t)) x) *empty-type-env*)
          simple-error "type-of-let 7")
(is-error (mini-lang::type-of-let '(let ((x vec3 1d0)) x) *empty-type-env*)
          simple-error "type-of-let 8")

(is (let ((type-env (mini-lang::add-type-environment 'x 'bool
                                                     *empty-type-env*)))
      (mini-lang::type-of-variable 'x type-env))
    'bool "type-of-variable 1")
(is (let ((type-env (mini-lang::add-type-environment 'x 'int
                                                     *empty-type-env*)))
      (mini-lang::type-of-variable 'x type-env))
    'int "type-of-variable 2")
(is (let ((type-env (mini-lang::add-type-environment 'x 'scalar
                                                     *empty-type-env*)))
      (mini-lang::type-of-variable 'x type-env))
    'scalar "type-of-variable 3")
(is (let ((type-env (mini-lang::add-type-environment 'x 'vec3
                                                     *empty-type-env*)))
      (mini-lang::type-of-variable 'x type-env))
    'vec3 "type-of-variable 4")
(is-error (mini-lang::type-of-variable 'x *empty-type-env*)
          simple-error "type-of-variable 5")

(is (mini-lang::type-of-if '(if t 2d0 1d0) *empty-type-env*)
    'scalar "type-of-if 1")
(is-error (mini-lang::type-of-if '(if t 2d0 (1d0 1d0 1d0)) *empty-type-env*)
          simple-error "type-of-if 2")
(is-error (mini-lang::type-of-if '(if 1d0 2d0 1d0) *empty-type-env*)
          simple-error "type-of-if 3")

(clear-functions)
(define-function f ((scalar x))
  (+ x 1d0))
(is (mini-lang::type-of-user-defined-application '(f 1d0))
    'scalar "type-of-user-defined-application 1")
(define-function g ((vec3 x))
  (+ x (1d0 1d0 1d0)))
(is (mini-lang::type-of-user-defined-application '(g (1d0 1d0 1d0)))
    'vec3 "type-of-user-defined-application 2")

(is (mini-lang::type-of-built-in-application '(= 1 1) *empty-type-env*)
    'bool "type-of-built-in-application 1")
(is (mini-lang::type-of-built-in-application '(* 1d0 1d0) *empty-type-env*)
    'scalar "type-of-built-in-application 2")
(is (mini-lang::type-of-built-in-application '(* (1d0 1d0 1d0) 1d0)
                                             *empty-type-env*)
    'vec3 "type-of-built-in-application 3")
(is-error (mini-lang::type-of-built-in-application '(++ 1d0 1d0)
                                                   *empty-type-env*)
          simple-error "type-of-built-in-application 4")
(is-error (mini-lang::type-of-built-in-application '(* (1d0 1d0 1d0)
                                                     (1d0 1d0 1d0))
                                                   *empty-type-env*)
          simple-error "type-of-built-in-application 5")


;;; test type environment

(is (mini-lang::empty-type-environment) '() "empty-type-environment")
(is (mini-lang::add-type-environment 'x 'scalar
                                     (mini-lang::empty-type-environment))
    '((x . scalar)) "add-type-environment")
(is (mini-lang::lookup-type-environment 'x '((x . scalar))) 'scalar
    "lookup-type-environment 1")
(is (mini-lang::lookup-type-environment 'x (mini-lang::empty-type-environment))
    nil "lookup-type-environment 2")
(is (mini-lang::make-type-environment '(x y) '(scalar vec3))
    '((y . vec3) (x . scalar)) "make-type-environment 1")
(is (mini-lang::make-type-environment '(x y) '(scalar))
    '((x . scalar)) "make-type-environment 2")
(is (mini-lang::make-type-environment '(x) '(scalar vec3))
    '((x . scalar)) "make-type-environment 3")


;;; test variable environment

(is (mini-lang::empty-variable-environment) '() "empty-variable-environment")
(is (mini-lang::add-variable-environment
     'x 'x1 (mini-lang::empty-variable-environment))
    '((x . x1)) "add-variable-environment")
(is (mini-lang::lookup-variable-environment 'x '((x . x1)))
    'x1 "lookup-variable-environment 1")
(is (mini-lang::lookup-variable-environment
     'x (mini-lang::empty-variable-environment))
    nil "lookup-variable-environment 2")
(is (mini-lang::make-variable-environment '(x y) '(x1 (y2 y3 y4)))
    '((y . (y2 y3 y4)) (x . x1)) "make-variable-environment 1")
(is (mini-lang::make-variable-environment '(x y) '(x1))
    '((x . x1)) "make-variable-environment 2")
(is (mini-lang::make-variable-environment '(x) '(x1 (y2 y3 y4)))
    '((x . x1)) "make-variable-environment 3")



;;; test binrization

(is (mini-lang::binarize '(+ 1 1 1)) '(+ (+ 1 1) 1) "binarize 1")
(is (mini-lang::binarize '(fn 1 1 1)) '(fn 1 1 1) "binarize 2")
(is (mini-lang::binarize '(fn (+ 1 1 1))) '(fn (+ (+ 1 1) 1)) "binarize 3")
(is (mini-lang::binarize '()) '() "binarize 4")
(is (mini-lang::binarize '(+ (+ 1 1 1) (+ 1 1 1) 1))
    '(+ (+ (+ (+ 1 1) 1)
         (+ (+ 1 1) 1))
      1) "binarize 4")
(is (mini-lang::binarize '(+ (+ 1 1 1) (+ 1 1 1)))
    '(+ (+ (+ 1 1) 1)
      (+ (+ 1 1) 1)) "binarize 5")
(is (mini-lang::type-of-mini-lang '(+ 1d0 1d0 1d0)) 'scalar "binarize 6")


;;; test single assignment

(mini-lang::reset-unique-variables-counter)
(is (mini-lang::single-assignment 't *empty-var-env* *empty-type-env*)
    't "single-assignment 1")
(is (mini-lang::single-assignment '1d0 *empty-var-env* *empty-type-env*)
    '1d0 "single-assignment 2")
(is (mini-lang::single-assignment '(1d0 1d0 1d0)
                                  *empty-var-env* *empty-type-env*)
    '(1d0 1d0 1d0) "single-assignment 3")
(is (mini-lang::single-assignment '(scalar x) *empty-var-env* *empty-type-env*)
    '(scalar x) "single-assignment 4")
(is (mini-lang::single-assignment '(let ((x scalar 1d0)) 1d0)
                                  *empty-var-env* *empty-type-env*)
    '(let ((x1 scalar 1d0)) 1d0) "single-assignment 5")
(is (mini-lang::single-assignment '(let ((x vec3 (1d0 1d0 1d0))) (1d0 1d0 1d0))
                                  *empty-var-env* *empty-type-env*)
    '(let ((x2 vec3 (1d0 1d0 1d0))) (1d0 1d0 1d0)) "single-assignment 6")
(is (mini-lang::single-assignment '(if (let ((x bool t)) t)
                                    (let ((x scalar 1d0)) 1d0)
                                    (let ((x scalar 2d0)) 2d0))
                                  *empty-var-env* *empty-type-env*)
    '(if (let ((x3 bool t)) t)
      (let ((x4 scalar 1d0)) 1d0)
      (let ((x5 scalar 2d0)) 2d0)) "single-assignment 7")
(is (mini-lang::single-assignment '(let ((x scalar 1d0)) x)
                                  *empty-var-env* *empty-type-env*)
    '(let ((x6 scalar 1d0)) x6) "single-assignment 8")
(is (mini-lang::single-assignment '(let ((x vec3 (1d0 1d0 1d0))) x)
                                  *empty-var-env* *empty-type-env*)
    '(let ((x7 vec3 (1d0 1d0 1d0))) x7) "single-assignment 9")
(is (mini-lang::single-assignment '(f 1d0) *empty-var-env* *empty-type-env*)
    '(f 1d0) "single-assignment 10")
(is (mini-lang::single-assignment '(f (let ((x scalar 1d0)) x))
                                  *empty-var-env* *empty-type-env*)
    '(f (let ((x8 scalar 1d0)) x8)) "single-assignment 11")
(is (mini-lang::single-assignment '(+ 1d0 1d0)
                                  *empty-var-env* *empty-type-env*)
    '(+ 1d0 1d0) "single-assignment 12")
(is (mini-lang::single-assignment '(+ 1d0 (let ((x scalar 1d0)) x))
                                  *empty-var-env* *empty-type-env*)
    '(+ 1d0 (let ((x9 scalar 1d0)) x9)) "single-assignment 13")


(finalize)
