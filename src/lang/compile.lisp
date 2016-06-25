#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.compile
  (:use :cl
        :mini-lang.lang.data
        :mini-lang.lang.type
        :mini-lang.lang.built-in
        :mini-lang.lang.syntax
        :mini-lang.lang.type-environment
        :mini-lang.lang.variable-environment)
  (:export :compile-form))
(in-package :mini-lang.lang.compile)


;;
;; Compile form

(defun compile-form (form venv tenv)
  (cond
    ((literal-p form) (compile-literal form venv tenv))
    ((reference-p form) (compile-reference form venv tenv))
    ((the-p form) (compile-the form venv tenv))
    ((if-p form) (compile-if form venv tenv))
    ((let-p form) (compile-let form venv tenv))
    ((application-p form) (compile-application form venv tenv))
    (t (error "The value ~S is an invalid form." form))))

(defun compile-literal (form venv tenv)
  (declare (ignore venv tenv))
  (cond
    ((int-literal-p form) (values form 'int))
    ((float-literal-p form) (values form 'float))
    ((double-literal-p form) (values form 'double))
    (t (error "Must not be reached."))))

(defun compile-reference (form venv tenv)
  (let* ((type (query-type-environment form tenv))
         (form1 (cond
                  ((scalar-type-p type)
                   (if (variable-environment-exists-p form venv)
                       (query-variable-environment form venv)
                       `(the ,(compile-type type) ,form)))
                  ((vector-type-p type)
                   (if (variable-environment-exists-p form venv)
                       (let ((vector-values* (vector-type-values* type))
                             (vars (query-variable-environment form venv)))
                         `(,vector-values* ,@vars))
                       (error
                        "Vector types do not support external reference.")))
                  ((array-type-p type)
                   (if (variable-environment-exists-p form venv)
                       (query-variable-environment form venv)
                       `(the ,(compile-type type) ,form)))
                  (t (error "Must not be reached.")))))
    (values form1 type)))

(defun compile-the (form venv tenv)
  (let ((form1 (the-form form)))
    (compile-form form1 venv tenv)))

(defun compile-if (form venv tenv)
  (let ((test-form (if-test-form form))
        (then-form (if-then-form form))
        (else-form (if-else-form form)))
    (multiple-value-bind (test-form1 _)
        (compile-form test-form venv tenv)
      (declare (ignore _))
      (multiple-value-bind (then-form1 _)
          (compile-form then-form venv tenv)
        (declare (ignore _))
        (multiple-value-bind (else-form1 type)
            (compile-form else-form venv tenv)
          (values `(if ,test-form1 ,then-form1 ,else-form1) type))))))

(defun compile-let (form venv tenv)
  (flet ((aux (ret binding)
           (destructuring-bind (fn venv1 tenv1) ret
             (destructuring-bind (var form) binding
               (multiple-value-bind (form1 type)
                   (compile-form form venv tenv)
                 (let ((tenv2 (extend-type-environment var type tenv1)))
                   (multiple-value-bind (venv2 var1)
                       (extend-variable-environment var type venv1)
                     (cond
                       ((or (scalar-type-p type)
                            (array-type-p type))
                        (let ((fn1 #'(lambda (form)
                                       (funcall fn `(let ((,var1 ,form1))
                                                      ,form)))))
                          (list fn1 venv2 tenv2)))
                       ((vector-type-p type)
                        (let ((fn1 #'(lambda (form)
                                       (funcall fn
                                        `(multiple-value-bind ,var1 ,form1
                                           ,form)))))
                        (list fn1 venv2 tenv2)))
                       (t (error "Must not be reached."))))))))))
    (let ((bindings (let-bindings form))
          (body (let-body form)))
      (destructuring-bind (fn venv1 tenv1)
          (reduce #'aux bindings :initial-value (list #'identity venv tenv))
        (multiple-value-bind (form1 type)
            (compile-form body venv1 tenv1)
          (values (funcall fn form1) type))))))

(defun compile-application (form venv tenv)
  (let ((operator (application-operator form))
        (operands (application-operands form)))
    (let ((argc (built-in-argc operator)))
      (unless (= argc (length operands))
        (error "Invalid number of arguments: ~S" (length operands))))
    (multiple-value-bind (operands1 argtypes)
        (let (operands1 argtypes)
          (loop for operand in operands
             do (multiple-value-bind (operand1 argtype)
                    (compile-form operand venv tenv)
                  (push operand1 operands1)
                  (push argtype argtypes)))
          (values (nreverse operands1) (nreverse argtypes)))
      (let ((operator1 (built-in-operator operator argtypes))
            (type (built-in-return-type operator argtypes)))
        (cond
          ((scalar-type-p type)
           (values `(the ,(compile-type type) (,operator1 ,@operands1)) type))
          ((vector-type-p type)
           (values `(,operator1 ,@operands1) type))
          ((array-type-p type)
           (values `(the ,(compile-type type) (,operator1 ,@operands1)) type))
          (t (error "Must not be reached.")))))))

(defun compile-type (type)
  (cl-pattern:match type
    ('int 'fixnum)
    ('float 'single-float)
    ('double 'double-float)
    ((:vector _ _) (error "Not implemented."))
    ((:array type1)
     (cl-pattern:match type1
       ('int 'int-array)
       ('float 'float-array)
       ('double 'double-array)
       ((:vector 'int 2) 'int2-array)
       ((:vector 'int 3) 'int3-array)
       ((:vector 'int 4) 'int4-array)
       ((:vector 'float 2) 'float2-array)
       ((:vector 'float 3) 'float3-array)
       ((:vector 'float 4) 'float4-array)
       ((:vector 'double 2) 'double2-array)
       ((:vector 'double 3) 'double3-array)
       ((:vector 'double 4) 'double4-array)
       (_ (error "Must not be reached."))))
    (_ (error "Must not be reached."))))
