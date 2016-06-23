#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.unification-environment
  (:use :cl
        :mini-lang.lang.type)
  (:export :empty-unification-environment
           :query-unification-environment
           :unify))
(in-package :mini-lang.lang.unification-environment)


;;
;; Unification environment

(defun empty-unification-environment ()
  (list nil nil (cl-unification:make-empty-environment)))

(defun query-unification-environment (type uenv)
  (check-type type mini-lang-type)
  (cond
    ((scalar-type-p type) type)
    ((vector-type-p type)
     (let ((base-type (vector-type-base-type type))
           (size (vector-type-size type)))
       (let ((base-type1 (query-unification-environment base-type uenv))
             (size1 (%query-unification-environment size uenv)))
         `(:vector ,base-type1 ,size1))))
    ((array-type-p type)
     (let ((base-type (array-type-base-type type)))
       (let ((base-type1 (query-unification-environment base-type uenv)))
         `(:array ,base-type1))))
    ((type-variable-p type)
     (destructuring-bind (lhs rhs subst) uenv
       (declare (ignore lhs rhs))
       (let ((type1 (cl-unification:find-variable-value type subst)))
         (if type1
             (query-unification-environment type1 uenv)
             type))))
    (t (error "Must not be reached."))))

(defun %query-unification-environment (size uenv)
  ;; For vector type size.
  (assert (mini-lang.lang.type::vector-type-size-p size))
  (if (type-variable-p size)
      (destructuring-bind (lhs rhs subst) uenv
        (declare (ignore lhs rhs))
        (let ((size1 (cl-unification:find-variable-value size subst)))
          (if size1
              (%query-unification-environment size1 uenv)
              size)))
      size))

(defun unify (type1 type2 uenv)
  (check-type type1 mini-lang-type)
  (check-type type2 mini-lang-type)
  (destructuring-bind (lhs rhs subst) uenv
    (let* ((lhs1 (cons type1 lhs))
           (rhs1 (cons type2 rhs))
           (subst1 (handler-case
                       (cl-unification:unify lhs1 rhs1 subst)
                     (cl-unification:unification-failure ()
                       subst))))
      (let* ((uenv1 (list lhs1 rhs1 subst1))
             (type (query-unification-environment type1 uenv1)))
        (values type uenv1)))))
