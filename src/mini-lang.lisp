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
           :vec3
           :scalar-aref
           :vec3-aref
           :setf-scalar                 ; operation interfaces
           :incf-scalar
           :setf-vec3
           :incf-vec3
           :defvar-scalar-array         ; operation interfaces for arrays
           :for-scalar-array
           :setf-scalar-array
           :incf-scalar-array
           :defvar-vec3-array
           :for-vec3-array
           :setf-vec3-array
           :incf-vec3-array
           :scalar                      ; scalar and scalar array
           :scalar-array
           :make-scalar-array
           :scalar-aref
           :scalar-array-size
           :vec3                        ; vec3 and vec3 array
           :make-vec3
           :vec3-array
           :make-vec3-array
           :vec3-aref
           :vec3-aref*
           :vec3-array-size
           :norm                        ; built in functions
           ))
(in-package :mini-lang)


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
            `(,op (,op ,(binarize a1) ,(binarize a2))
                  ,@(mapcar #'binarize rest)))
          (destructuring-bind (op . rest) exp
            `(,op ,@(mapcar #'binarize rest))))))


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


;;; definition of vec3

(deftype vec3 () '(simple-array double-float (3)))
(deftype vec3-array () '(simple-array double-float (*)))

(def-tuple-type vec3
    :tuple-element-type double-float
    :initial-element 0d0
    :elements (x y z))

(declaim (ftype (function (vec3-array) fixnum) vec3-array-size))
(defun vec3-array-size (x)
  (vec3-array-dimensions x))

(defmacro vec3-negate* (x)
  `(vec3-scale* ,x -1d0))

(def-tuple-op vec3-add*
  ((veca vec3 (x1 y1 z1))
   (vecb vec3 (x2 y2 z2)))
  (:return vec3
           (vec3-values* (+ x1 x2) (+ y1 y2) (+ z1 z2))))

(def-tuple-op vec3-sub*
  ((veca vec3 (x1 y1 z1))
   (vecb vec3 (x2 y2 z2)))
  (:return vec3
           (vec3-values* (- x1 x2) (- y1 y2) (- z1 z2))))

(def-tuple-op vec3-scale*
  ((vec vec3 (x y z))
   (k   double-float (k)))
  (:return vec3
           (vec3-values* (* x k) (* y k) (* z k))))

(defmacro vec3-scale%* (k x)
  `(vec3-scale* ,x ,k))

(defmacro vec3-scale-recip* (x k)
  `(vec3-scale* ,x (/ 1d0 ,k)))

(def-tuple-op vec3-norm*
  ((vec vec3 (x y z)))
  (:return double-float
           (sqrt (+ (* x x) (* y y) (* z z)))))
           

;;; operation interface

(defmacro defvar-scalar-array (&rest rest)
  `(progn (declaim (type scalar-array ,@rest))
          ,@(mapcar (lambda (x)
                      `(defvar ,x)) rest)))

(defmacro defvar-vec3-array (&rest rest)
  `(progn (declaim (type vec3-array ,@rest))
          ,@(mapcar (lambda (x)
                      `(defvar ,x)) rest)))

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

(defmacro setf-vec3 (place exp)
  (let ((type (type-of-mini-lang exp)))
    (if (eq type 'vec3)
        `(setf ,(expand-vec3-place place) (compile-mini-lang ,exp))
        (error (format nil "invalid type of expression: ~A" exp)))))

(defmacro incf-vec3 (place exp)
  `(setf-vec3 ,place (+ ,place ,exp)))

(defun expand-vec3-place (place)
  (if (variable-p place)
      `(vec3* ,place)
      (match place
        (('vec3-aref x i) `(vec3-aref* ,x ,i))
        (_ (error (format nil "invalid vec3 place: ~A" place))))))

(defmacro for-vec3-array (x i &rest body)
  `(macrolet ((setf-vec3-array (exp)
                `(setf-vec3 (vec3-aref ,',x ,',i) ,exp))
              (incf-vec3-array (exp)
                `(incf-vec3 (vec3-aref ,',x ,',i) ,exp)))
     (dotimes (,i (vec3-array-size ,x))
       ,@body)))


;;; compile

(defmacro compile-mini-lang (exp)
  (compile-exp (binarize exp) (empty-type-environment)))

(defun compile-exp (exp type-env)
  (cond ((scalar-literal-p exp) exp)
        ((vec3-literal-p exp) (compile-vec3-literal exp))
        ((external-environment-reference-p exp)
         (compile-external-environment-reference exp))
        ((let-p exp) (compile-let exp type-env))
        ((variable-p exp) (compile-variable exp type-env))
        ((application-p exp) (compile-application exp type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))


;;; scalar literal

(defun scalar-literal-p (exp)
  (typep exp 'double-float))


;;; vec3 literal

(defun vec3-literal-p (exp)
  (match exp
    ((x y z) (every #'scalar-literal-p (list x y z)))
    (_       nil)))

(defun compile-vec3-literal (exp)
  (match exp
    ((x y z) `(vec3-values* ,x ,y ,z))))


;;; external environment reference

(defun external-environment-reference-p (exp)
  (match exp
    (('scalar _) t)
    (('vec3 _) t)
    (('scalar-aref _ _) t)
    (('vec3-aref _ _) t)
    (_ nil)))

(defun compile-external-environment-reference (exp)
  (match exp
    (('scalar x) `(the scalar ,x))
    (('vec3 x) `(vec3* ,x))
    (('scalar-aref x i) `(scalar-aref ,x ,i))
    (('vec3-aref x i) `(vec3-aref* ,x ,i))))


;;; let expression

;;
;; (let ((x 1d0)
;;       (y (1d0 1d0 1d0)))
;;   ...)
;; =>
;; (let ((x 1d0))
;;   (multiple-value-bind (y0 y1 y2) (vec3-values* 1d0 1d0 1d0))
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
        ((_ 'vec3 _) (compile-vec3-bind binds exp type-env)))))

(defun compile-scalar-bind (binds exp type-env)
  (match binds
    (((var 'scalar val) . rest)
      (let ((type-env2 (add-type-environment var 'scalar type-env)))
        `(let ((,var ,(compile-exp val type-env)))
           ,(compile-let% rest exp type-env2))))))

(defun compile-vec3-bind (binds exp type-env)
  (match binds
    (((var 'vec3 val) . rest)
      (let ((type-env2 (add-type-environment var 'vec3 type-env)))
        (multiple-value-bind (x y z) (make-symbols-for-values var)
          `(multiple-value-bind (,x ,y ,z) ,(compile-exp val type-env)
             ,(compile-let% rest exp type-env2)))))))


;;; variable

;;
;; when type of exp is scalar:
;;   (let ((x 1d0))
;;     x)
;; when thpe of exp is vec3:
;;   (multiple-value-bind (x0 x1 x2) (vec3-values* 1d0 1d0 1d0)
;;     (vec3-values* x0 x1 x2))
;;

(defun variable-p (exp)
  (symbolp exp))

(defun compile-variable (var type-env)
  (cond ((scalar-type-p var type-env) var)
        ((vec3-type-p var type-env) (multiple-value-bind (x y z)
                                          (make-symbols-for-values var)
                                        `(vec3-values* ,x ,y ,z)))))

(defun make-symbols-for-values (s)
  (values (symb s 0) (symb s 1) (symb s 2)))


;;; function application

(defvar built-in-functions              ; constant
  '(+ (((scalar scalar) scalar +)
       ((vec3 vec3) vec3 vec3-add*))
    - (((scalar) scalar -)
       ((vec3) vec3 vec3-negate*)
       ((scalar scalar) scalar -)
       ((vec3 vec3) vec3 vec3-sub*))
    * (((scalar scalar) scalar *)
       ((vec3 scalar) vec3 vec3-scale*)
       ((scalar vec3) vec3 vec3-scale%*))
    / (((scalar scalar) scalar /)
       ((vec3 scalar) vec3 vec3-scale-recip*))
    norm (((vec3) scalar vec3-norm*))
    exp (((scalar) scalar exp))))

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
;;  ((vec3 vec3) vec3 vec3-add*))
(defun operation-candidates (op)
  (getf built-in-functions op))


;;; type

(defun type-of-mini-lang (exp)
  (type-of-exp (binarize exp) (empty-type-environment)))

(defun type-of-exp (exp type-env)
  (cond ((scalar-literal-p exp) 'scalar)
        ((vec3-literal-p exp) 'vec3)
        ((external-environment-reference-p exp)
         (type-of-external-environment-reference exp))
        ((let-p exp) (type-of-let exp type-env))
        ((variable-p exp) (type-of-variable exp type-env))
        ((application-p exp) (type-of-application exp type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))

(defun scalar-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'scalar))

(defun vec3-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'vec3))

(defun type-of-external-environment-reference (exp)
  (match exp
    (('scalar _) 'scalar)
    (('vec3 _) 'vec3)
    (('scalar-aref _ _) 'scalar)
    (('vec3-aref _ _) 'vec3)))

(defun type-of-let (exp type-env)
  (type-of-let% (let-binds exp) (let-exp exp) type-env))

(defun type-of-let% (binds exp type-env)
  (if (null binds)
      (type-of-exp exp type-env)
      (match (car binds)
        ((_ 'scalar _) (type-of-scalar-bind binds exp type-env))
        ((_ 'vec3 _) (type-of-vec3-bind binds exp type-env)))))

(defun type-of-scalar-bind (binds exp type-env)
  (match binds
    (((var 'scalar _) . rest)
      (let ((type-env2 (add-type-environment var 'scalar type-env)))
        (type-of-let% rest exp type-env2)))))

(defun type-of-vec3-bind (binds exp type-env)
  (match binds
    (((var 'vec3 _) . rest)
      (let ((type-env2 (add-type-environment var 'vec3 type-env)))
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
               (eq ,type 'vec3))))

(defun add-type-environment (var type type-env)
  (assert-type type)
  (cons (cons var type) type-env))

(defun lookup-type-environment (var type-env)
  (match (assoc var type-env)
    ((_ . type) type)
    (_          nil)))


