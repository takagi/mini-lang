#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang
  (:use :cl
        :cl-pattern
        :cl-tuples-wrapper
        :anaphora
        :alexandria)
  (:export :bool                        ; external environment references
           :int
           :scalar
           :vec3
           :scalar-aref
           :vec3-aref
           :setf-scalar                 ; operation interfaces
           :incf-scalar
           :setf-vec3
           :incf-vec3
           :for-scalar-array            ; operation interfaces for arrays
           :setf-scalar-array
           :incf-scalar-array
           :for-vec3-array
           :setf-vec3-array
           :incf-vec3-array
           :bool                        ; bool type
           :int                         ; int type
           :scalar                      ; scalar and scalar array
           :scalar-array
           :make-scalar-array
           :scalar-aref
           :scalar-array-size
           :vec3                        ; vec3 and vec3 array
           :vec3-x :vec3-y :vec3-z
           :with-vec3
           :make-vec3
           :vec3-array
           :with-vec3-aref
           :make-vec3-array
           :vec3-aref
           :vec3-array-size
           :clear-functions             ; interface for user-defined functions
           :define-function
           :norm                        ; built in functions
           :dot
           ))
(in-package :mini-lang)


;;; mini-lang form transformation

(defun binarize (exp)
  (if (atom exp)
      exp
      (if (and (nthcdr 3 exp)
               (member (car exp) '(+ - * /)))
          (destructuring-bind (op a1 a2 . rest) exp
            (binarize `(,op (,op ,(binarize a1) ,(binarize a2)) ,@rest)))
          (destructuring-bind (op . rest) exp
            `(,op ,@(mapcar #'binarize rest))))))

(defun single-assignment (exp var-env type-env)
  (cond ((bool-literal-p exp) exp)
        ((int-literal-p exp) exp)
        ((scalar-literal-p exp) exp)
        ((vec3-literal-p exp) exp)
        ((external-environment-reference-p exp) exp)
        ((let-p exp) (single-assignment-let exp var-env type-env))
        ((if-p exp) (single-assignment-if exp var-env type-env))
        ((variable-p exp) (single-assignment-variable exp var-env))
        ((user-defined-application-p exp)
         (single-assignment-user-defined-application exp var-env type-env))
        ((built-in-application-p exp)
         (single-assignment-built-in-application exp var-env type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))

(defun single-assignment-let (exp var-env type-env)
  (single-assignment-let% (let-binds exp) (let-exp exp) var-env type-env))

(defun single-assignment-let% (binds exp var-env type-env)
  (if (null binds)
      (single-assignment exp var-env type-env)
      (single-assignment-bind binds exp var-env type-env)))

(defun single-assignment-bind (binds exp var-env type-env)
  (match binds
    (((var type val) . rest)
     (if (eq (type-of-exp val type-env) type)
         (let* ((unique-var (make-unique-variable var))
                (var-env2 (add-variable-environment var unique-var var-env))
                (type-env2 (add-type-environment var type type-env)))
           `(let ((,unique-var ,type ,(single-assignment val var-env type-env)))
              ,(single-assignment-let% rest exp var-env2 type-env2)))
         (error (format nil "contradict type in let bind: ~A" var))))))

(defun single-assignment-if (exp var-env type-env)
  (match exp
    (('if test-form then-form else-form)
     `(if ,(single-assignment test-form var-env type-env)
          ,(single-assignment then-form var-env type-env)
          ,(single-assignment else-form var-env type-env)))))

(defun single-assignment-variable (var var-env)
  (aif (lookup-variable-environment var var-env)
       it
       (error (format nil "unbound variable: ~A" var))))

(defun single-assignment-user-defined-application (exp var-env type-env)
  (labels ((same-type (arg val)
             (eq (user-defined-function-arg-type arg)
                 (type-of-exp val type-env))))
    (match exp
      ((fun . vals)
       (let ((args (user-defined-function-args fun)))
         (if (= (length args) (length vals))
             (if (every #'same-type args vals)
                 `(,fun ,@(mapcar (lambda (val)
                                    (single-assignment val var-env type-env))
                                  vals))
                 (error (format nil "invalid argument type: ~A" exp)))
             (error (format nil "invalid number of arguments: ~A" exp))))))))

(defun single-assignment-built-in-application (exp var-env type-env)
  (match exp
    ((op . operands)
     (let ((candidates (operation-candidates op)))
       (if (infer-op operands candidates type-env)
           `(,op ,@(mapcar (lambda (val)
                             (single-assignment val var-env type-env))
                           operands))
           (error (format nil "invalid application: ~A" exp)))))))
                           

;;; definition of bool

(deftype bool () 'boolean)


;;; definition of int

(deftype int () 'fixnum)


;;; definition of scalar

(deftype scalar () 'double-float)
(deftype scalar-array () '(simple-array double-float (*)))

(defun make-scalar-array (n)
  (make-array n :element-type 'double-float :initial-element 0d0))

(defmacro scalar-aref (x i)
  `(aref (the scalar-array ,x) ,i))

(declaim (ftype (function (scalar-array) fixnum) scalar-array-size))
(defun scalar-array-size (x)
  (length x))


;;; definition of vec3

(deftype vec3 () '(simple-array double-float (3)))
(deftype vec3-array () '(simple-array double-float (*)))

(def-tuple-type vec3
    :tuple-element-type double-float
    :initial-element 0d0
    :elements (x y z)
    :simple-array t)

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
   (val double-float (val)))
  (:return vec3
           ; When this def-tuple-op macro is expanded, val is specified
           ; with symbol-macrolet so that this expression makes
           ; val evaluated three times which causing poor performance.
           ;
           ;   (vec3-values* (* x val) (* y val) (* z val))))
           ;
           ; To avoid it, val is once bound to k and k is used instead of val
           ; to make val evaluated only once.
           (let ((k val))
             (vec3-values* (* x k) (* y k) (* z k)))))

(defmacro vec3-scale%* (k x)
  `(vec3-scale* ,x ,k))

(defmacro vec3-scale-recip* (x k)
  `(vec3-scale* ,x (/ 1d0 ,k)))

(def-tuple-op vec3-norm*
  ((vec vec3 (x y z)))
  (:return double-float
           (sqrt (+ (* x x) (* y y) (* z z)))))

(def-tuple-op vec3-dot*
  ((veca vec3 (x1 y1 z1))
   (vecb vec3 (x2 y2 z2)))
  (:return double-float
           (+ (* x1 x2) (* y1 y2) (* z1 z2))))


;;; operation interface

(defmacro setf-scalar (var exp)
  (let ((type (type-of-mini-lang exp)))
    (if (eq type 'scalar)
        `(setf ,var (compile-mini-lang ,exp))
        (error (format nil "invalid type of expression: ~A" exp)))))

(defmacro incf-scalar (var exp)
  `(setf-scalar ,var (+ ,var ,exp)))

(defmacro setf-scalar-array (var i exp)
  (let ((type (type-of-mini-lang exp)))
    (if (eq type 'scalar)
        `(setf (scalar-aref ,var ,i) (compile-mini-lang ,exp))
        (error (format nil "invalid type of expression: ~A" exp)))))

(defmacro incf-scalar-array (var i exp)
  `(setf-scalar-array ,var ,i (+ (scalar-aref ,var ,i) ,exp)))

(defmacro for-scalar-array (x i &rest body)
  `(dotimes (,i (scalar-array-size ,x))
     ,@body))

(defun for-scalar-array% (x fun)
  (dotimes (i (scalar-array-size x))
    (funcall fun i)))

(defmacro setf-vec3 (var exp)
  (let ((type (type-of-mini-lang exp)))
    (if (eq type 'vec3)
        `(setf (vec3* ,var) (compile-mini-lang ,exp))
        (error (format nil "invalid type of expression: ~A" exp)))))

(defmacro incf-vec3 (var exp)
  `(setf-vec3 ,var (+ ,var ,exp)))

(defmacro setf-vec3-array (var i exp)
  (let ((type (type-of-mini-lang exp)))
    (if (eq type 'vec3)
        `(setf (vec3-aref* ,var ,i) (compile-mini-lang ,exp))
        (error (format nil "invalid type of expression: ~A" exp)))))

(defmacro incf-vec3-array (var i exp)
  `(setf-vec3-array ,var ,i (+ ,exp (vec3-aref ,var ,i))))

(defmacro for-vec3-array (x i &rest body)
  `(dotimes (,i (vec3-array-size ,x))
     ,@body))


;;; compile

(defmacro compile-mini-lang (exp)
  (let ((exp2 (single-assignment (binarize exp)
                                 (empty-variable-environment)
                                 (empty-type-environment))))
    (compile-exp exp2 (empty-type-environment))))

(defun compile-exp (exp type-env)
  (cond ((bool-literal-p exp) exp)
        ((int-literal-p exp) exp)
        ((scalar-literal-p exp) exp)
        ((vec3-literal-p exp) (compile-vec3-literal exp))
        ((external-environment-reference-p exp)
         (compile-external-environment-reference exp))
        ((let-p exp) (compile-let exp type-env))
        ((if-p exp) (compile-if exp type-env))
        ((variable-p exp) (compile-variable exp type-env))
        ((user-defined-application-p exp)
         (compile-user-defined-application exp type-env))
        ((built-in-application-p exp)
         (compile-built-in-application exp type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))


;;; bool literal

(defun bool-literal-p (exp)
  (or (eq exp 't)
      (eq exp 'nil)))


;;; int literal

(defun int-literal-p (exp)
  (typep exp 'fixnum))


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
    (('bool _) t)
    (('int _) t)
    (('scalar _) t)
    (('vec3 _) t)
    (('vec3 _ _ _) t)
    (('scalar-aref _ _) t)
    (('vec3-aref _ _) t)
    (_ nil)))

(defun compile-external-environment-reference (exp)
  (match exp
    (('bool x) x)
    (('int x) `(the int ,x))
    (('scalar x) `(the scalar ,x))
    (('vec3 x) `(vec3* ,x))
    (('vec3 x y z) `(vec3-values* ,x ,y ,z))
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
        ((_ 'bool _) (compile-single-bind 'bool binds exp type-env))
        ((_ 'int _) (compile-single-bind 'int binds exp type-env))
        ((_ 'scalar _) (compile-single-bind 'scalar binds exp type-env))
        ((_ 'vec3 _) (compile-vec3-bind binds exp type-env)))))

(defun compile-single-bind (type binds exp type-env)
  (match binds
    (((var _ val) . rest)
      (if (eq (type-of-exp val type-env) type)
          (let ((type-env2 (add-type-environment var type type-env)))
            `(let ((,var ,(compile-exp val type-env)))
               ,(compile-let% rest exp type-env2)))
          (error (format nil "contradict type in let bind: ~A" var))))))

(defun compile-vec3-bind (binds exp type-env)
  (match binds
    (((var 'vec3 val) . rest)
      (if (vec3-type-p val type-env)
          (let ((type-env2 (add-type-environment var 'vec3 type-env)))
            `(multiple-value-bind ,(make-symbols-for-values var)
                 ,(compile-exp val type-env)
               ,(compile-let% rest exp type-env2)))
          (error (format nil "contradict type in let bind: ~A" var))))))


;;; if expression

(defun if-p (exp)
  (match exp
    (('if . _) t)
    (_ nil)))

(defun compile-if (exp type-env)
  (match exp
    (('if test-form then-form else-form)
       `(if ,(compile-exp test-form type-env)
            ,(compile-exp then-form type-env)
            ,(compile-exp else-form type-env)))))


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
  (cond ((single-type-p var type-env) var)
        ((vec3-type-p var type-env) `(vec3-values*
                                      ,@(make-symbols-for-values var)))))

(defun make-symbols-for-values (s)
  (list (symbolicate s "-0") (symbolicate s "-1") (symbolicate s "-2")))


;;; definition and application of user-defined functions

(defvar *user-defined-functions* nil)

(defun clear-functions ()
  (setf *user-defined-functions* nil))

(defmacro define-function (name args exp)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((func (make-user-defined-function ',name ',args ',exp)))
       (setf (getf *user-defined-functions* ',name) func)
       ',name)))


;; <user-defined-functions> ::= ( <name> <user-defined-function>
;;                                <name> <user-defined-function> ... )
;; <user-defined-function>  ::= ( <name> <args> <return-type>
;;                                              <compiled-expression> )
;; <args>                   ::= ( <arg>* )
;; <arg>                    ::= ( <var> <type> )
;; <unique-vars>            ::= <var>          if <type> is bool, int and scalar
;;                            | ( <var> <var> <var> )  if <type> is vec3

(defun make-user-defined-function (name args exp)
  (labels ((valid-args (args)
             (and (listp args)
                  (every (lambda (arg)
                           (and (consp arg) (= (length arg) 2)))
                         args))))
    (if (valid-args args)
        (let* ((vars (mapcar #'cadr args))
               (types (mapcar #'car args))
               (unique-vars (mapcar #'make-unique-variable vars)))
          (let* ((var-env (make-variable-environment vars unique-vars))
                 (type-env (make-type-environment vars types))
                 (exp2 (single-assignment (binarize exp) var-env type-env)))
            (let ((type-env2 (make-type-environment unique-vars types)))
              (let ((args2 (make-user-defined-function-args unique-vars types))
                    (return-type (type-of-exp exp2 type-env2))
                    (compiled-expression (compile-exp exp2 type-env2)))
                (list name args2 return-type compiled-expression)))))
        (error (format nil "invalid function definition: (define-function ~A ~A ~A)" name args exp)))))

(defun make-user-defined-function-args (vars types)
  (mapcar #'list vars types))

(let ((counter 0))
  (defun make-unique-variable (var)
    (when (= counter most-positive-fixnum)
      (reset-unique-variables-counter))
    (symbolicate var (princ-to-string (incf counter))))
  (defun reset-unique-variables-counter ()
    (setf counter 0)))

(defun user-defined-function-name (fun)
  (match (getf *user-defined-functions* fun)
    ((name _ _ _) name)
    (_ (error (format nil "undefined function: ~A" fun)))))

(defun user-defined-function-args (fun)
  (match (getf *user-defined-functions* fun)
    ((_ args _ _) args)
    (_ (error (format nil "undefined function: ~A" fun)))))

(defun user-defined-function-return-type (fun)
  (match (getf *user-defined-functions* fun)
    ((_ _ return-type _) return-type)
    (_ (error (format nil "undefined function: ~A" fun)))))

(defun user-defined-function-compiled-expression (fun)
  (match (getf *user-defined-functions* fun)
    ((_ _ _ compiled-expression) compiled-expression)
    (_ (error (format nil "undefined function: ~A" fun)))))

(defun user-defined-function-arg-var (arg)
  (match arg
    ((var _) var)
    (_ (error (format nil "invalid argument: ~A" arg)))))

(defun user-defined-function-arg-type (arg)
  (match arg
    ((_ type) type)
    (_ (error (format nil "invalid argument: ~A" arg)))))

(defun user-defined-application-p (exp)
  (match exp
    ((fun . _) (not (null (getf *user-defined-functions* fun))))
    (_ nil)))

(defun compile-user-defined-application (exp type-env)
  (labels ((same-type (arg val)
             (eq (user-defined-function-arg-type arg)
                 (type-of-exp val type-env))))
    (match exp
      ((fun . vals)
       (let ((args (user-defined-function-args fun))
             (exp2 (user-defined-function-compiled-expression fun)))
         (if (= (length args) (length vals))
             (if (every #'same-type args vals)
                 (compile-user-defined-application%
                  args vals exp2 type-env)
                 (error (format nil "invalid argument type: ~A" exp)))
             (error (format nil "invalid number of arguments: ~A" exp))))))))

(defun compile-user-defined-application% (args vals exp type-env)
  (if (null args)
      exp
      (let ((arg (car args))
            (val (car vals))
            (rest-args (cdr args))
            (rest-vals (cdr vals)))
        (case (user-defined-function-arg-type arg)
          (bool (compile-user-defined-application-single%
                 arg val rest-args rest-vals exp type-env))
          (int (compile-user-defined-application-single%
                arg val rest-args rest-vals exp type-env))
          (scalar (compile-user-defined-application-single%
                   arg val rest-args rest-vals exp type-env))
          (vec3 (compile-user-defined-application-vec3%
                 arg val rest-args rest-vals exp type-env))))))

(defun compile-user-defined-application-single% (arg val args vals exp type-env)
  (let ((var (user-defined-function-arg-var arg)))
    `(let ((,var ,(compile-exp val type-env)))
       ,(compile-user-defined-application% args vals exp type-env))))

(defun compile-user-defined-application-vec3% (arg val args vals exp type-env)
  (let ((var (user-defined-function-arg-var arg)))
    `(multiple-value-bind ,(make-symbols-for-values var)
         ,(compile-exp val type-env)
       ,(compile-user-defined-application% args vals exp type-env))))


;;; application of built-in functions

(defvar *built-in-functions*              ; constant
  '(vec3-x (((vec3) scalar vec3-x*))
    vec3-y (((vec3) scalar vec3-y*))
    vec3-z (((vec3) scalar vec3-z*))
    + (((int int) int +)
       ((scalar scalar) scalar +)
       ((vec3 vec3) vec3 vec3-add*))
    - (((int) int -)
       ((scalar) scalar -)
       ((vec3) vec3 vec3-negate*)
       ((int int) int -)
       ((scalar scalar) scalar -)
       ((vec3 vec3) vec3 vec3-sub*))
    * (((int int) int *)
       ((scalar scalar) scalar *)
       ((vec3 scalar) vec3 vec3-scale*)
       ((scalar vec3) vec3 vec3-scale%*))
    / (((scalar scalar) scalar /)
       ((vec3 scalar) vec3 vec3-scale-recip*))
    norm (((vec3) scalar vec3-norm*))
    dot (((vec3 vec3) scalar vec3-dot*))
    exp (((scalar) scalar exp))
    expt (((scalar int) scalar expt))
    = (((int int) bool =))
    <= (((scalar scalar) bool <=))
    > (((scalar scalar) bool >))
    debug (((bool) bool debug%)
           ((int) int debug%)
           ((scalar) scalar debug%)
           ((vec3) vec3 debug%))))

(defun debug% (x)
  (format t "~A~%" x)
  x)

(defun built-in-application-p (exp)
  (match exp
    ((op . _) (not (null (getf *built-in-functions* op))))
    (_ nil)))

(defun compile-built-in-application (exp type-env)
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
  (getf *built-in-functions* op))


;;; type

(defun type-of-mini-lang (exp)
  (type-of-exp (binarize exp) (empty-type-environment)))

(defun type-of-exp (exp type-env)
  (cond ((bool-literal-p exp) 'bool)
        ((int-literal-p exp) 'int)
        ((scalar-literal-p exp) 'scalar)
        ((vec3-literal-p exp) 'vec3)
        ((external-environment-reference-p exp)
         (type-of-external-environment-reference exp))
        ((let-p exp) (type-of-let exp type-env))
        ((if-p exp) (type-of-if exp type-env))
        ((variable-p exp) (type-of-variable exp type-env))
        ((user-defined-application-p exp)
         (type-of-user-defined-application exp))
        ((built-in-application-p exp)
         (type-of-built-in-application exp type-env))
        (t (error (format nil "invalid expression: ~A" exp)))))

(defun bool-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'bool))

(defun int-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'int))

(defun scalar-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'scalar))

(defun single-type-p (exp type-env)
  (or (bool-type-p exp type-env)
      (int-type-p exp type-env)
      (scalar-type-p exp type-env)))

(defun vec3-type-p (exp type-env)
  (eq (type-of-exp exp type-env) 'vec3))

(defun type-of-external-environment-reference (exp)
  (match exp
    (('bool _) 'bool)
    (('int _) 'int)
    (('scalar _) 'scalar)
    (('vec3 _) 'vec3)
    (('vec3 _ _ _) 'vec3)
    (('scalar-aref _ _) 'scalar)
    (('vec3-aref _ _) 'vec3)))

(defun type-of-let (exp type-env)
  (type-of-let% (let-binds exp) (let-exp exp) type-env))

(defun type-of-let% (binds exp type-env)
  (if (null binds)
      (type-of-exp exp type-env)
      (match (car binds)
        ((_ 'bool _) (type-of-single-bind 'bool binds exp type-env))
        ((_ 'int _) (type-of-single-bind 'int binds exp type-env))
        ((_ 'scalar _) (type-of-single-bind 'scalar binds exp type-env))
        ((_ 'vec3 _) (type-of-vec3-bind binds exp type-env)))))

(defun type-of-single-bind (type binds exp type-env)
  (match binds
    (((var _ val) . rest)
      (if (eq (type-of-exp val type-env) type)
          (let ((type-env2 (add-type-environment var type type-env)))
            (type-of-let% rest exp type-env2))
          (error (format nil "contradict type in let bind: ~A" var))))))

(defun type-of-vec3-bind (binds exp type-env)
  (match binds
    (((var 'vec3 val) . rest)
      (if (vec3-type-p val type-env)
          (let ((type-env2 (add-type-environment var 'vec3 type-env)))
            (type-of-let% rest exp type-env2))
          (error (format nil "contradict type in let bind: ~A" var))))))

(defun type-of-if (exp type-env)
  (match exp
    (('if test-form then-form else-form)
       (let ((type-of-test-form (type-of-exp test-form type-env))
             (type-of-then-form (type-of-exp then-form type-env))
             (type-of-else-form (type-of-exp else-form type-env)))
         (cond ((not (eq type-of-test-form 'bool))
                (error (format nil
                               "type of the test form is not bool: ~A" exp)))
               ((not (eq type-of-then-form type-of-else-form))
                (error
                 (format nil
                         "type of the then and else forms are not same: ~A"
                         exp)))
               (t type-of-then-form))))))

(defun type-of-variable (var type-env)
  (aif (lookup-type-environment var type-env)
       it
       (error (format nil "unbound variable: ~A" var))))

(defun type-of-user-defined-application (exp)
  (match exp
    ((fun . _) (user-defined-function-return-type fun))))

(defun type-of-built-in-application (exp type-env)
  (match exp
    ((op . operands) (let ((candidates (operation-candidates op)))
                       (aif (infer-return-type operands candidates type-env)
                            it
                            (error (format nil
                                           "invalid application: ~A" exp)))))))


;;; type environment

;; type-environment ::= ( <type-pair>* )
;; type-pair        ::= ( <variable> . <type> )

(defun empty-type-environment ()
  '())

(defmacro assert-type (type)
  `(assert (member ,type '(bool int scalar vec3))))

(defun add-type-environment (var type type-env)
  (assert-type type)
  (cons (cons var type) type-env))

(defun make-type-environment (vars types)
  (let ((pairs (mapcar #'cons vars types)))
    (reduce (lambda (env pair)
              (add-type-environment (car pair) (cdr pair) env))
            pairs :initial-value (empty-type-environment))))

(defun lookup-type-environment (var type-env)
  (match (assoc var type-env)
    ((_ . type) type)
    (_ nil)))


;;; variable environment

;; variable-environment ::= ( <variable-pair>* )
;; variable-pair        ::= ( <variable> . <unique-variable> )

(defun empty-variable-environment ()
  '())

(defun add-variable-environment (var unique-var var-env)
  (cons (cons var unique-var) var-env))

(defun make-variable-environment (vars unique-vars)
  (let ((pairs (mapcar #'cons vars unique-vars)))
    (reduce (lambda (env pair)
              (add-variable-environment (car pair) (cdr pair) env))
            pairs :initial-value (empty-variable-environment))))

(defun lookup-variable-environment (var var-env)
  (match (assoc var var-env)
    ((_ . unique-var) unique-var)
    (_ nil)))
