#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.free-variables
  (:use :cl
        :mini-lang.lang.syntax)
  (:export :free-variables))
(in-package :mini-lang.lang.free-variables)


;;
;; Free variables

(defun free-variables (form env ret)
  (cond
    ((literal-p form) (free-variables-literal form env ret))
    ((reference-p form) (free-variables-reference form env ret))
    ((the-p form) (free-variables-the form env ret))
    ((if-p form) (free-variables-if form env ret))
    ((let-p form) (free-variables-let form env ret))
    ((application-p form) (free-variables-application form env ret))
    (t (error "The value ~S is an invalid form." form))))

(defun free-variables-literal (form env ret)
  (declare (ignore form env))
  ret)

(defun free-variables-reference (form env ret)
  (if (member form env)
      ret
      (cons form ret)))

(defun free-variables-the (form env ret)
  (let ((form1 (the-form form)))
    (free-variables form1 env ret)))

(defun free-variables-if (form env ret)
  (let ((test-form (if-test-form form))
        (then-from (if-then-form form))
        (else-form (if-else-form form)))
    (free-variables else-form env
     (free-variables then-from env
      (free-variables test-form env ret)))))

(defun free-variables-let (form env ret)
  (flet ((aux (env-ret binding)
           (destructuring-bind (env1 ret1) env-ret
             (destructuring-bind (var1 form1) binding
               (let ((env2 (cons var1 env1))
                     (ret2 (free-variables form1 env ret1)))
                 (list env2 ret2))))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (destructuring-bind (env1 ret1)
          (reduce #'aux bindings :initial-value (list env ret))
        (free-variables body env1 ret1)))))

(defun free-variables-application (form env ret)
  (flet ((aux (ret operand)
           (free-variables operand env ret)))
    (let ((operands (application-operands form)))
      (reduce #'aux operands :initial-value ret))))
