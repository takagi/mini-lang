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


;;; test compile-exp

(is (mini-lang::compile-exp 1d0 nil) 1d0)
(is (mini-lang::compile-exp '(1d0 1d0 1d0) nil) '(values 1d0 1d0 1d0))


;;; test literal

(is (mini-lang::scalar-literal-p '1d0) t)

(is (mini-lang::vector-literal-p '(1d0 1d0 1d0)) t)
(is (mini-lang::compile-vector-literal '(1d0 1d0 1d0)) '(values 1d0 1d0 1d0))


;;; test let expression

(is (mini-lang::let-p '(let)) t)
(is (mini-lang::let-binds '(let ((x scalar 1d0)) x)) '((x scalar 1d0)))
(is (mini-lang::let-exp '(let ((x scalar 1d0)) x)) 'x)
(is (mini-lang::compile-let% '((x scalar 1d0)) '1d0 nil)
    '(let ((x 1d0))
       1d0))
(is (let ((type-env (mini-lang::add-type-environment
                      'x 'scalar (mini-lang::empty-type-environment))))
      (mini-lang::compile-let% '((x scalar 1d0)) 'x type-env))
    '(let ((x 1d0))
       x))
(is (mini-lang::compile-let% '((x vector (1d0 1d0 1d0))) '1d0 nil)
    '(multiple-value-bind (x0 x1 x2) (values 1d0 1d0 1d0)
       1d0))
(is (let ((type-env (mini-lang::add-type-environment
                      'x 'vector (mini-lang::empty-type-environment))))
      (mini-lang::compile-let% '((x vector (1d0 1d0 1d0))) 'x type-env))
    '(multiple-value-bind (x0 x1 x2) (values 1d0 1d0 1d0)
       (values x0 x1 x2)))
(is (mini-lang::compile-let '(let ((x scalar 1d0))
                              x)
                            (mini-lang::empty-type-environment))
    '(let ((x 1d0))
       x))
(is (mini-lang::compile-let '(let ((x vector (1d0 1d0 1d0)))
                              x)
                            (mini-lang::empty-type-environment))
    '(multiple-value-bind (x0 x1 x2) (values 1d0 1d0 1d0)
       (values x0 x1 x2)))
(is (mini-lang::compile-let '(let ((y scalar 1d0))
                               (let ((x vector (1d0 1d0 1d0)))
                                 y))
                            (mini-lang::empty-type-environment))
    '(let ((y 1d0))
       (multiple-value-bind (x0 x1 x2) (values 1d0 1d0 1d0)
         y)))


;;; test variable

(is (mini-lang::variable-p 'x) t)
(let* ((type-env (mini-lang::add-type-environment 'y 'vector
                   (mini-lang::add-type-environment 'x 'scalar
                     (mini-lang::empty-type-environment)))))
  (is (mini-lang::compile-variable 'x type-env) 'x)
  (is (mini-lang::compile-variable 'y type-env) '(values y0 y1 y2)))

(multiple-value-bind (a b c) (mini-lang::make-symbols-for-values 'x)
  (is a 'x0)
  (is b 'x1)
  (is c 'x2))


;;; test function application

(is (mini-lang::application-p '(+ 1d0 1d0)) t)
(is (mini-lang::application-p '(++ 1d0 1d0)) nil)

(is (mini-lang::compile-application '(+ 1d0 1d0) nil)
    '(+ 1d0 1d0))
(is (let ((type-env (mini-lang::add-type-environment 'x 'scalar
                      (mini-lang::add-type-environment 'y 'scalar
                        (mini-lang::empty-type-environment)))))
      (mini-lang::compile-application '(+ x y) type-env))
    '(+ x y))
(is (mini-lang::compile-application '(+ (1d0 1d0 1d0) (1d0 1d0 1d0)) nil)
    '(mini-lang::vector-+ (values 1d0 1d0 1d0) (values 1d0 1d0 1d0)))
(is (let ((type-env (mini-lang::add-type-environment 'x 'vector
                      (mini-lang::add-type-environment 'y 'vector
                        (mini-lang::empty-type-environment)))))
      (mini-lang::compile-application '(+ x y) type-env))
    '(mini-lang::vector-+ (values x0 x1 x2) (values y0 y1 y2)))

(is-error (mini-lang::compile-application '(++ 1d0 1d0) nil)
          simple-error)
(is-error (mini-lang::compile-application '(+ (1d0 1d0 1d0) 1d0) nil)
          simple-error)

(is (mini-lang::compile-application '(* (1d0 1d0 1d0) 1d0) nil)
    '(mini-lang::vector-*. (values 1d0 1d0 1d0) 1d0))
(is (mini-lang::compile-application '(* 1d0 (1d0 1d0 1d0)) nil)
    '(mini-lang::vector-.* 1d0 (values 1d0 1d0 1d0)))

(is (mini-lang::operand-types '(x y) '((x . scalar) (y . scalar)))
    '(scalar scalar))
(is (mini-lang::infer-op '(x y)
                         (mini-lang::operation-candidates '+)
                         '((x . vector) (y . vector)))
    'mini-lang::vector-+)
(is (mini-lang::infer-return-type '(x y)
                                  (mini-lang::operation-candidates '+)
                                  '((x . vector) (y . vector)))
    'vector)


;;; test type

(is (mini-lang::type-of-exp '1d0 nil) 'scalar)
(is (mini-lang::type-of-exp '(1d0 1d0 1d0) nil) 'vector)
(is (let ((type-env (mini-lang::add-type-environment
                      'x 'scalar (mini-lang::empty-type-environment))))
      (mini-lang::type-of-exp 'x type-env)) 'scalar)
(is (mini-lang::type-of-let '(let ((x scalar 1d0)) x) nil) 'scalar)
(is (let ((type-env (mini-lang::add-type-environment
                      'y 'vector (mini-lang::empty-type-environment))))
      (mini-lang::type-of-let '(let ((x scalar 1d0)) y) type-env)) 'vector)
(is (let ((type-env (mini-lang::add-type-environment
                      'x 'scalar (mini-lang::empty-type-environment))))
      (mini-lang::type-of-variable 'x type-env)) 'scalar)
(is-error (mini-lang::type-of-variable 'x nil) simple-error)
(is (mini-lang::type-of-application '(* 1d0 1d0) nil) 'scalar)
(is (mini-lang::type-of-application '(* (1d0 1d0 1d0) 1d0) nil) 'vector)
(is-error (mini-lang::type-of-application '(++ 1d0 1d0) nil) simple-error)
(is-error (mini-lang::type-of-application '(* (1d0 1d0 1d0) (1d0 1d0 1d0)) nil)
                                          simple-error)


;;; test type environment

(is (mini-lang::empty-type-environment) '())
(is (mini-lang::add-type-environment 'x 'scalar
                                     (mini-lang::empty-type-environment))
    '((x . scalar)))
(is (mini-lang::lookup-type-environment 'x '((x . scalar))) 'scalar)
(is (mini-lang::lookup-type-environment 'x '()) nil)


(finalize)
