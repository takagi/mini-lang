#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang
  (:use :cl
        :mini-lang.lang.type
        :mini-lang.lang.built-in
        :mini-lang.lang.unification-environment
        :mini-lang.lang.type-environment
        :mini-lang.lang.variable-environment
        :mini-lang.lang.infer
        :mini-lang.lang.compile
        :mini-lang.lang.free-variables)
  (:export :eval-mini-lang
           :compile-mini-lang))
(in-package :mini-lang.lang)


;;
;; Eval

(defmacro eval-mini-lang (form)
  (eval `(compile-mini-lang ',form)))


;;
;; Compile

(defun compile-mini-lang (form)
  (flet ((aux (tenv var)
           (extend-type-environment var (gentype) tenv)))
    (let ((form1 (binarize form)))
      (let* ((free-vars (free-variables form nil nil))
             (tenv (reduce #'aux free-vars
                           :initial-value (empty-type-environment)))
             (uenv (empty-unification-environment)))
        (multiple-value-bind (_ uenv1) (infer form1 tenv uenv)
          (declare (ignore _))
          (let ((venv (empty-variable-environment))
                (tenv1 (substitute-type-environment uenv1 tenv)))
            (compile-form form1 venv tenv1)))))))

(defun binarize (form)
  (if (atom form)
      form
      (if (and (nthcdr 3 form)
               (built-in-arithmetic-p (car form)))
          (if (built-in-arithmetic-left-assoc-p (car form))
              (destructuring-bind (op a1 a2 . rest) form
                (binarize `(,op (,op ,(binarize a1) ,(binarize a2)) ,@rest)))
              (destructuring-bind (op . args) form
                (destructuring-bind (a2 a1 . rest) (reverse args)
                  (binarize `(,op ,@(reverse rest)
                                  (,op ,(binarize a1) ,(binarize a2)))))))
          (destructuring-bind (op . rest) form
            `(,op ,@(mapcar #'binarize rest))))))
