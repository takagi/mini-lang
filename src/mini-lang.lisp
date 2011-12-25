#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang
  (:use :cl
        :cl-pattern
        :cl-tuples
        :lol)
  (:export :compile-mini-lang
           :scalar                      ; external environment references
           :vector
           :scalar-aref
           :vector-aref
           :setf-scalar                 ; operation interfaces
           :incf-scalar
           :setf-vector
           :incf-vector
           :for-scalar-array            ; operation interfaces for arrays
           :setf-scalar-array
           :incf-scalar-array
           :for-vector-array
           :setf-vector-array
           :incf-vector-array
           :scalar                      ; scalar and scalar array
           :scalar-array
           :make-scalar-array
           :scalar-aref
           :scalar-array-size
           :vector%                     ; vector and vector array
           :make-vector
           :vector-array
           :make-vector-array
           :vector-aref
           :vector-aref*
           :vector-array-size
           :it                          ; for aif that appears in expanded macro
           ))
(in-package :mini-lang)


;;; definition of scalar

(deftype scalar () 'double-float)
(deftype scalar-array () '(simple-array double-float (*)))

(defun make-scalar-array (n)
  (make-array n :element-type 'double-float :initial-element 0d0))

(defmacro scalar-aref (x i)
  `(aref ,x ,i))

(declaim (ftype (function (scalar-array) fixnum) scalar-array-size))
(defun scalar-array-size (x)
  (length x))


;;; definition of vector

(deftype vector% () '(simple-array double-float (3)))
(deftype vector-array () '(simple-array double-float (*)))

(declaim (ftype (function (vector-array) fixnum) vector-array-size))
(defun vector-array-size (x)
  (vector-array-dimensions x))

(def-tuple-type vector
    :tuple-element-type double-float
    :initial-element 0d0
    :elements (x y z))

(def-tuple-op vector-add*
  ((veca vector (x1 y1 z1))
   (vecb vector (x2 y2 z2)))
  (:return vector
           (vector-values* (+ x1 x2) (+ y1 y2) (+ z1 z2))))

(def-tuple-op vector-sub*
  ((veca vector (x1 y1 z1))
   (vecb vector (x2 y2 z2)))
  (:return vector
           (vector-values* (- x1 x2) (- y1 y2) (- z1 z2))))

(def-tuple-op vector-scale*
  ((vec vector (x y z))
   (k   double-float (k)))
  (:return vector
           (vector-values* (* x k) (* y k) (* z k))))

(def-tuple-op vector-scale%*
  ((k   double-float (k))
   (vec vector (x y z)))
  (:return vector
           (vector-values* (* x k) (* y k) (* z k))))

(def-tuple-op vector-scale-recip*
  ((vec vector (x y z))
   (k   double-float (k)))
  (:return vector
           (vector-values* (/ x k) (/ y k) (/ z k))))


;;; operation interface

(defmacro setf-scalar (place exp)
  (let ((type (type-of-mini-lang exp)))
    (if (eq type 'scalar)
        `(setf ,(expand-scalar-place place) (compile-mini-lang ,exp))
        (error (format nil "invalid type of expression: ~A" exp)))))

(defmacro incf-scalar (place exp)
  `(setf-scalar ,place (+ ,place ,exp)))

(defun expand-scalar-place (place)
  (if (variable-p place)
      place
      (match place
        (('scalar-aref x i) `(scalar-aref ,x ,i))
        (_ (error (format nil "invalid scalar place: ~A" place))))))

(defmacro for-scalar-array (x i &rest body)
  `(macrolet ((setf-scalar-array (exp)
                `(setf-scalar (scalar-aref ,',x ,',i) ,exp))
              (incf-scalar-array (exp)
                `(incf-scalar (scalar-aref ,',x ,',i) ,exp)))
     (dotimes (,i (scalar-array-size ,x))
       ,@body)))

(defmacro setf-vector (place exp)
  (let ((type (type-of-mini-lang exp)))
    (if (eq type 'vector)
        `(setf ,(expand-vector-place place) (compile-mini-lang ,exp))
        (error (format nil "invalid type of expression: ~A" exp)))))

(defmacro incf-vector (place exp)
  `(setf-vector ,place (+ ,place ,exp)))

(defun expand-vector-place (place)
  (if (variable-p place)
      `(vector* ,place)
      (match place
        (('vector-aref x i) `(vector-aref* ,x ,i))
        (_ (error (format nil "invalid vector place: ~A" place))))))

(defmacro for-vector-array (x i &rest body)
  `(macrolet ((setf-vector-array (exp)
                `(setf-vector (vector-aref ,',x ,',i) ,exp))
              (incf-vector-array (exp)
                `(incf-vector (vector-aref ,',x ,',i) ,exp)))
     (dotimes (,i (vector-array-size ,x))
       ,@body)))


;;; compile

(defmacro compile-mini-lang (exp)
  (compile-exp (binarize exp) (empty-type-environment)))

(defun compile-exp (exp type-env)
  (cond ((scalar-literal-p exp) exp)
        ((vector-literal-p exp) (compile-vector-literal exp))
        ((external-environment-reference-p exp)
         (compile-external-environment-reference exp))
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
    ((x y z) `(vector-values* ,x ,y ,z))))


;;; external environment reference

(defun external-environment-reference-p (exp)
  (match exp
    (('scalar _) t)
    (('vector _) t)
    (('scalar-aref _ _) t)
    (('vector-aref _ _) t)
    (_ nil)))

(defun compile-external-environment-reference (exp)
  (match exp
    (('scalar x) x)
    (('vector x) `(vector* ,x))
    (('scalar-aref x i) `(scalar-aref ,x ,i))
    (('vector-aref x i) `(vector-aref* ,x ,i))))


;;; let expression

;;
;; (let ((x 1d0)
;;       (y (1d0 1d0 1d0)))
;;   ...)
;; =>
;; (let ((x 1d0))
;;   (multiple-value-bind (y0 y1 y2) (vector-values* 1d0 1d0 1d0))
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
;;   (multiple-value-bind (x0 x1 x2) (vector-values* 1d0 1d0 1d0)
;;     (vector-values* x0 x1 x2))
;;

(defun variable-p (exp)
  (symbolp exp))

(defun compile-variable (var type-env)
  (cond ((scalar-type-p var type-env) var)
        ((vector-type-p var type-env) (multiple-value-bind (x y z)
                                          (make-symbols-for-values var)
                                        `(vector-values* ,x ,y ,z)))))

(defun make-symbols-for-values (s)
  (values (symb s 0) (symb s 1) (symb s 2)))


;;; function application

(defconstant built-in-functions
  '(+ (((scalar scalar) scalar +)
       ((vector vector) vector vector-add*))
    - (((scalar scalar) scalar -)
       ((vector vector) vector vector-sub*))
    * (((scalar scalar) scalar *)
       ((vector scalar) vector vector-scale*)
       ((scalar vector) vector vector-scale%*))
    / (((scalar scalar) scalar /)
       ((vector scalar) vector vector-scale-recip*))))

(defun application-p (exp)
  (match exp
    ((op . _) (not (null (getf built-in-functions op))))
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
;;  ((vector vector) vector vector-add*))
(defun operation-candidates (op)
  (getf built-in-functions op))


;;; type

(defun type-of-mini-lang (exp)
  (type-of-exp exp (empty-type-environment)))

(defun type-of-exp (exp type-env)
  (cond ((scalar-literal-p exp) 'scalar)
        ((vector-literal-p exp) 'vector)
        ((external-environment-reference-p exp)
         (type-of-external-environment-reference exp))
        ((let-p exp) (type-of-let exp type-env))
        ((variable-p exp) (type-of-variable exp type-env))
        ((application-p exp) (type-of-application exp type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))

(defun scalar-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'scalar))

(defun vector-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'vector))

(defun type-of-external-environment-reference (exp)
  (match exp
    (('scalar _) 'scalar)
    (('vector _) 'vector)
    (('scalar-aref _ _) 'scalar)
    (('vector-aref _ _) 'vector)))

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

(defmacro assert-type (type)
  `(assert (or (eq ,type 'scalar)
               (eq ,type 'vector))))

(defun add-type-environment (var type type-env)
  (assert-type type)
  (cons (cons var type) type-env))

(defun lookup-type-environment (var type-env)
  (match (assoc var type-env)
    ((_ . type) type)
    (_          nil)))


;;; utilities

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun binarize (exp)
  (if (atom exp)
      exp
      (if (and (nthcdr 3 exp)
               (member (car exp) '(+ - * /)))
          (destructuring-bind (op a1 a2 . rest) exp
            (binarize `(,op (,op ,a1 ,a2) ,@rest)))
          (destructuring-bind (op . rest) exp
            `(,op ,@(mapcar #'binarize rest))))))
