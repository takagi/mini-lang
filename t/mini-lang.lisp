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


;;; test operation interface

(is (mini-lang::expand-scalar-place 'x) 'x)
(is (mini-lang::expand-scalar-place '(scalar-aref x i)) '(scalar-aref x i))

(is (mini-lang::expand-vec3-place 'x) '(mini-lang::vec3* x))
(is (mini-lang::expand-vec3-place '(vec3-aref x i))
    '(mini-lang::vec3-aref* x i))

(is-expand (setf-scalar x 1d0) (setf x (compile-mini-lang 1d0)))
(is-expand (setf-scalar (scalar-aref x i) 1d0)
           (setf (scalar-aref x i) (compile-mini-lang 1d0)))

(is-expand (incf-scalar x 1d0) (setf-scalar x (+ x 1d0)))
(is-expand (incf-scalar (scalar-aref x i) 1d0)
           (setf-scalar (scalar-aref x i) (+ (scalar-aref x i) 1d0)))

(is-expand (setf-vec3 x (1d0 1d0 1d0))
           (setf (mini-lang::vec3* x)
                 (compile-mini-lang (1d0 1d0 1d0))))
(is-expand (setf-vec3 (vec3-aref x i) (1d0 1d0 1d0))
           (setf (mini-lang::vec3-aref* x i)
                 (compile-mini-lang (1d0 1d0 1d0))))

(is-expand (incf-vec3 x (1d0 1d0 1d0)) (setf-vec3 x (+ x (1d0 1d0 1d0))))
(is-expand (incf-vec3 (vec3-aref x i) (1d0 1d0 1d0))
           (setf-vec3 (vec3-aref x i) (+ (vec3-aref x i) (1d0 1d0 1d0))))

(is-expand (for-scalar-array x i
             (setf-scalar-array 1d0))
           (macrolet ((setf-scalar-array (exp)
                        `(setf-scalar (scalar-aref x i) ,exp))
                      (incf-scalar-array (exp)
                        `(incf-scalar (scalar-aref x i) ,exp)))
             (dotimes (i (scalar-array-size x))
               (setf-scalar-array 1d0))))

(is-expand (for-vec3-array x i
             (setf-vec3-array (1d0 1d0 1d0)))
           (macrolet ((setf-vec3-array (exp)
                        `(setf-vec3 (vec3-aref x i) ,exp))
                      (incf-vec3-array (exp)
                        `(incf-vec3 (vec3-aref x i) ,exp)))
             (dotimes (i (vec3-array-size x))
               (setf-vec3-array (1d0 1d0 1d0)))))

(is (let ((x (make-scalar-array 1)))
      (for-scalar-array x i
        (setf-scalar-array 1d0)
        (incf-scalar-array 1d0))
      (scalar-aref x 0))
    2d0)

(is (let ((x (make-vec3-array 1)))
      (for-vec3-array x i
        (setf-vec3-array (1d0 1d0 1d0))
        (incf-vec3-array (1d0 1d0 1d0)))
      (mini-lang::vec3-aref* x 0))
    (mini-lang::vec3-values* 2d0 2d0 2d0))


;;; test compile-exp

(is (mini-lang::compile-exp 1d0 nil) 1d0)
(is (mini-lang::compile-exp '(1d0 1d0 1d0) nil)
    '(mini-lang::vec3-values* 1d0 1d0 1d0))


;;; test literal

(is (mini-lang::scalar-literal-p '1d0) t)

(is (mini-lang::vec3-literal-p '(1d0 1d0 1d0)) t)
(is (mini-lang::compile-vec3-literal '(1d0 1d0 1d0))
    '(mini-lang::vec3-values* 1d0 1d0 1d0))


;;; test external environment reference

(is (mini-lang::external-environment-reference-p '(scalar x)) t)
(is (mini-lang::external-environment-reference-p '(vec3 x)) t)
(is (mini-lang::external-environment-reference-p '(scalar-aref x i)) t)
(is (mini-lang::external-environment-reference-p '(vec3-aref x i)) t)
(is (mini-lang::external-environment-reference-p '(scalar x y)) nil)

(is (mini-lang::compile-external-environment-reference '(scalar x))
    '(the scalar x))
(is (mini-lang::compile-external-environment-reference '(vec3 x))
    '(mini-lang::vec3* x))
(is (mini-lang::compile-external-environment-reference '(scalar-aref x i))
    '(scalar-aref x i))
(is (mini-lang::compile-external-environment-reference '(vec3-aref x i))
    '(mini-lang::vec3-aref* x i))


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
(is (mini-lang::compile-let% '((x vec3 (1d0 1d0 1d0))) '1d0 nil)
    '(multiple-value-bind (x0 x1 x2) (mini-lang::vec3-values* 1d0 1d0 1d0)
       1d0))
(is (let ((type-env (mini-lang::add-type-environment
                      'x 'vec3 (mini-lang::empty-type-environment))))
      (mini-lang::compile-let% '((x vec3 (1d0 1d0 1d0))) 'x type-env))
    '(multiple-value-bind (x0 x1 x2) (mini-lang::vec3-values* 1d0 1d0 1d0)
       (mini-lang::vec3-values* x0 x1 x2)))
(is (mini-lang::compile-let '(let ((x scalar 1d0))
                              x)
                            (mini-lang::empty-type-environment))
    '(let ((x 1d0))
       x))
(is (mini-lang::compile-let '(let ((x vec3 (1d0 1d0 1d0)))
                              x)
                            (mini-lang::empty-type-environment))
    '(multiple-value-bind (x0 x1 x2) (mini-lang::vec3-values* 1d0 1d0 1d0)
       (mini-lang::vec3-values* x0 x1 x2)))
(is (mini-lang::compile-let '(let ((y scalar 1d0))
                               (let ((x vec3 (1d0 1d0 1d0)))
                                 y))
                            (mini-lang::empty-type-environment))
    '(let ((y 1d0))
       (multiple-value-bind (x0 x1 x2) (mini-lang::vec3-values* 1d0 1d0 1d0)
         y)))


;;; test variable

(is (mini-lang::variable-p 'x) t)
(let* ((type-env (mini-lang::add-type-environment 'y 'vec3
                   (mini-lang::add-type-environment 'x 'scalar
                     (mini-lang::empty-type-environment)))))
  (is (mini-lang::compile-variable 'x type-env) 'x)
  (is (mini-lang::compile-variable 'y type-env)
      '(mini-lang::vec3-values* y0 y1 y2)))

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
    '(mini-lang::vec3-add* (mini-lang::vec3-values* 1d0 1d0 1d0)
                             (mini-lang::vec3-values* 1d0 1d0 1d0)))
(is (let ((type-env (mini-lang::add-type-environment 'x 'vec3
                      (mini-lang::add-type-environment 'y 'vec3
                        (mini-lang::empty-type-environment)))))
      (mini-lang::compile-application '(+ x y) type-env))
    '(mini-lang::vec3-add* (mini-lang::vec3-values* x0 x1 x2)
                             (mini-lang::vec3-values* y0 y1 y2)))

(is-error (mini-lang::compile-application '(++ 1d0 1d0) nil)
          simple-error)
(is-error (mini-lang::compile-application '(+ (1d0 1d0 1d0) 1d0) nil)
          simple-error)

(is (mini-lang::compile-application '(* (1d0 1d0 1d0) 1d0) nil)
    '(mini-lang::vec3-scale* (mini-lang::vec3-values* 1d0 1d0 1d0) 1d0))
(is (mini-lang::compile-application '(* 1d0 (1d0 1d0 1d0)) nil)
    '(mini-lang::vec3-scale%* 1d0 (mini-lang::vec3-values* 1d0 1d0 1d0)))

(is (mini-lang::operand-types '(x y) '((x . scalar) (y . scalar)))
    '(scalar scalar))
(is (mini-lang::infer-op '(x y)
                         (mini-lang::operation-candidates '+)
                         '((x . vec3) (y . vec3)))
    'mini-lang::vec3-add*)
(is (mini-lang::infer-return-type '(x y)
                                  (mini-lang::operation-candidates '+)
                                  '((x . vec3) (y . vec3)))
    'vec3)

(is (mini-lang::compile-application '(norm (1d0 1d0 1d0)) nil)
    '(mini-lang::vec3-norm* (mini-lang::vec3-values* 1d0 1d0 1d0)))


;;; test type

(is (mini-lang::type-of-exp '1d0 nil) 'scalar)
(is (mini-lang::type-of-exp '(1d0 1d0 1d0) nil) 'vec3)
(is (let ((type-env (mini-lang::add-type-environment
                      'x 'scalar (mini-lang::empty-type-environment))))
      (mini-lang::type-of-exp 'x type-env)) 'scalar)

(is (mini-lang::type-of-external-environment-reference '(scalar x)) 'scalar)
(is (mini-lang::type-of-external-environment-reference '(vec3 x)) 'vec3)
(is (mini-lang::type-of-external-environment-reference '(scalar-aref x i))
    'scalar)
(is (mini-lang::type-of-external-environment-reference '(vec3-aref x i))
    'vec3)

(is (mini-lang::type-of-let '(let ((x scalar 1d0)) x) nil) 'scalar)
(is (let ((type-env (mini-lang::add-type-environment
                      'y 'vec3 (mini-lang::empty-type-environment))))
      (mini-lang::type-of-let '(let ((x scalar 1d0)) y) type-env)) 'vec3)

(is (let ((type-env (mini-lang::add-type-environment
                      'x 'scalar (mini-lang::empty-type-environment))))
      (mini-lang::type-of-variable 'x type-env)) 'scalar)
(is-error (mini-lang::type-of-variable 'x nil) simple-error)

(is (mini-lang::type-of-application '(* 1d0 1d0) nil) 'scalar)
(is (mini-lang::type-of-application '(* (1d0 1d0 1d0) 1d0) nil) 'vec3)
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


;;; test utilities

(is (mini-lang::binarize '(+ 1 1 1)) '(+ (+ 1 1) 1))
(is (mini-lang::binarize '(fn 1 1 1)) '(fn 1 1 1))
(is (mini-lang::binarize '(fn (+ 1 1 1))) '(fn (+ (+ 1 1) 1)))
(is (mini-lang::binarize '()) '())


(finalize)
