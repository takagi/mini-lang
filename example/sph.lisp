#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :cl-user)
(defpackage mini-lang-example.sph
  (:use :cl
        :cl-pattern
        :mini-lang
        :alexandria)
  (:export :main))
(in-package :mini-lang-example.sph)


;;; Constants

(defconstant nx 6)
(defconstant ny 10)
(defconstant nz 10)
(defconstant n (* nx ny nz))
(defconstant dt 0.004d0)
(defconstant h 0.01d0)
(defconstant simscale 4d-3)
(defconstant pmass 0.00020543d0)
(defconstant restdensity 600.0d0)
(defconstant intstiff 3.0d0)
(defconstant pdist (expt (/ pmass restdensity) (/ 1 3)))
(defconstant visc 0.2d0)
(defconstant radius 0.004d0)
(defconstant extstiff 10000d0)
(defconstant extdamp 256d0)
(defconstant epsilon 0.00001d0)
(defconstant limit 200d0)
(defvar g (make-vec3 0.0d0 -9.8d0 0.0d0))    ; constant
(defvar box-min (make-vec3 0d0 0d0 -10d0))   ; constant
(defvar box-max (make-vec3 20d0 50d0 10d0))  ; constant
(defvar init-min (make-vec3 0d0 0d0 -10d0))  ; constant


;;; Arrays

(defvar-vec3-array *x* *v* *f*)
(defvar-scalar-array *rho* *prs*)


;;; Neighbor map

(defvar *nbr*)

(defstruct (neighbor-map
             (:constructor make-raw-neighbor-map
                           (min max delta size-x size-y size-z array)))
  (min (make-vec3 0d0 0d0 0d0) :type vec3)
  (max (make-vec3 0d0 0d0 0d0) :type vec3)
  (delta 0d0 :type scalar)
  (size-x 0 :type fixnum)
  (size-y 0 :type fixnum)
  (size-z 0 :type fixnum)
  array)

(defun make-neighbor-map (min max delta)
  (labels ((size (min max delta)
             (assert (and (< min max) (< 0 delta)))
             (multiple-value-bind (x y) (floor (/ (- max min) delta))
               (if (= y 0d0) x (1+ x)))))
    (let* ((i (size (vec3-x min) (vec3-x max) delta))
           (j (size (vec3-y min) (vec3-y max) delta))
           (k (size (vec3-z min) (vec3-z max) delta))
           (max2 (make-vec3 (+ (vec3-x min) (* delta i))
                            (+ (vec3-y min) (* delta j))
                            (+ (vec3-z min) (* delta k))))
           (array (make-array (* i j k) :initial-element nil)))
      (make-raw-neighbor-map min max2 delta i j k array))))

(declaim (ftype (function (neighbor-map fixnum fixnum fixnum) list)
                neighbor-map-particles))
(defun neighbor-map-particles (nbr i j k)
  (declare (optimize (speed 3) (safety 0)))
  (if (neighbor-map-valid-index nbr i j k)
      (let ((array (neighbor-map-array nbr)))
        (declare (type (simple-array list (*)) array))
        (aref array (neighbor-map-index nbr i j k)))
      nil))

(defun neighbor-map-index% (nbr i j k)
  (let ((size-x (neighbor-map-size-x nbr))
        (size-y (neighbor-map-size-y nbr)))
    (+ (* size-x size-y k)
       (* size-x j)
       i)))

(defun neighbor-map-index (nbr i j k)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum i j k))
  (let ((size-x (neighbor-map-size-x nbr))
        (size-y (neighbor-map-size-y nbr)))
    (declare (type fixnum size-x size-y))
    (the fixnum (+ (the fixnum (* size-x (the fixnum (* size-y k))))
                   (the fixnum (* size-x j))
                   i))))

(declaim (inline neighbor-map-valid-index))
(defun neighbor-map-valid-index (nbr i j k)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum i j k))  
  (and (<= 0 i) (<= 0 j) (<= 0 k)
       (< i (neighbor-map-size-x nbr))
       (< j (neighbor-map-size-y nbr))
       (< k (neighbor-map-size-z nbr))))

(defun clear-neighbor-map (nbr)
  (dotimes (i (neighbor-map-size-x nbr))
    (dotimes (j (neighbor-map-size-y nbr))
      (dotimes (k (neighbor-map-size-z nbr))
        (setf (aref (neighbor-map-array nbr) (neighbor-map-index nbr i j k))
              nil)))))

(declaim (ftype (function (neighbor-map vec3-array (signed-byte 32))
                          (values fixnum fixnum fixnum))
                neighbor-map-pos-to-index))
(defun neighbor-map-pos-to-index (nbr array index)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (x y z) (mini-lang::vec3-aref* array index)
    (neighbor-map-pos-to-index% nbr x y z)))

(declaim (ftype (function (neighbor-map scalar scalar scalar)
                          (values fixnum fixnum fixnum))
                neighbor-map-pos-to-index%))
(declaim (inline neighbor-map-pos-to-index%))
(defun neighbor-map-pos-to-index% (nbr x y z)
  (declare (optimize (speed 3) (safety 0)))
  (labels ((index (x0 x d)
             (let ((idx (/ (- x x0) d)))
               ; restrict the range of idx to make floor return fixnum,
               ;  not integer
               (declare (type (double-float -1d10 1d10) idx)) 
               (floor idx))))
    (let* ((min (neighbor-map-min nbr))
           (delta (neighbor-map-delta nbr))
           (i (index (vec3-x min) x delta))
           (j (index (vec3-y min) y delta))
           (k (index (vec3-z min) z delta)))
      (values i j k))))

(defun insert-particle (nbr pos idx)
  (multiple-value-bind (i j k) (neighbor-map-pos-to-index nbr pos idx)
    (when (neighbor-map-valid-index nbr i j k)
      (push idx (aref (neighbor-map-array nbr) (neighbor-map-index nbr i j k))))))

(defun update-neighbor-map (nbr xs)
  (clear-neighbor-map nbr)
  (for-vec3-array xs i
    (insert-particle nbr xs i)))

(defmacro for-neighbors-in-cell (nbr i j k var &rest body)
  `(dolist (,var (neighbor-map-particles ,nbr ,i ,j ,k))
     (declare (type (signed-byte 32) ,var))
     ,@body))

#|
(defmacro for-neighbors (nbr xs index var &rest body)
  (with-gensyms (i j k di dj dk)
    `(multiple-value-bind (,i ,j ,k)
         (neighbor-map-pos-to-index ,nbr ,xs ,index)
       (dolist (,di '(-1 0 1))
         (dolist (,dj '(-1 0 1))
           (dolist (,dk '(-1 0 1))
             (for-neighbors-in-cell ,nbr (+ ,i ,di) (+ ,j ,dj) (+ ,k ,dk) ,var
               ,@body)))))))
|#

(defmacro for-neighbors (nbr pos var &rest body)
  (match pos
    ((x y z) (with-gensyms (i j k)
               `(multiple-value-bind (,i ,j ,k)
                    (neighbor-map-pos-to-index% ,nbr ,x ,y ,z)
                  (for-neighbors% ,nbr ,i ,j ,k ,var ,@body))))
    ((xs idx) (with-gensyms (i j k)
                `(multiple-value-bind (,i ,j ,k)
                     (neighbor-map-pos-to-index ,nbr ,xs ,idx)
                   (for-neighbors% ,nbr ,i ,j ,k ,var ,@body))))
    ((val) (with-gensyms (x y z)
             `(multiple-value-bind (,x ,y ,z)
                  (mini-lang::vec3* ,val)
                (for-neighbors ,nbr ,x ,y ,z ,var ,@body))))))

(defmacro for-neighbors% (nbr i j k var &rest body)
  (with-gensyms (di dj dk)
    `(dolist (,di '(-1 0 1))
       (dolist (,dj '(-1 0 1))
         (dolist (,dk '(-1 0 1))
           (for-neighbors-in-cell ,nbr (+ ,i ,di) (+ ,j ,dj) (+ ,k ,dk) ,var
             ,@body))))))


;;; Print functions

(defun print-number-of-particles ()
  (format t "~A particles~%" N))

(defun print-step (i)
  (format t "processing ~A ...~%" (file-name i)))


;;; Output functions

(defun file-name (i)
  (format nil "result~8,'0d.pov" i))

(defun head ()
  (format nil (concatenate 'string
                           "#include \"colors.inc\"~%"
                           "camera {~%"
                           "  location <10, 30, -40>~%"
                           "  look_at <10, 10, 0>~%"
                           "}~%"
                           "light_source { <0, 30, -30> color White }~%")))

(defun sphere (i)
  (with-vec3-aref (*x* i (x y z))
    (format nil
            (concatenate 'string
                         "sphere {~%"
                         "  <~F,~F,~F>,0.5~%"
                         "  texture {~%"
                         "    pigment { color Yellow }~%"
                         "  }~%"
                         "}~%")
            x y z)))

(defun output (i)
  (with-open-file (out (file-name i)
                       :direction :output
                       :if-exists :supersede)
    (princ (head) out)
    (dotimes (i N)
      (princ (sphere i) out))))


;;; Main

(defun initialize ()
  (setf *x* (make-vec3-array N))
  (setf *v* (make-vec3-array N))
  (setf *f* (make-vec3-array N))
  (setf *rho* (make-scalar-array N))
  (setf *prs* (make-scalar-array N))
  (setf *nbr* (make-neighbor-map box-min box-max (/ h simscale)))
  (dotimes (x NX)
    (dotimes (y NY)
      (dotimes (z NZ)
        (let* ((d (* (/ pdist simscale) 0.95))
               (i (+ x (* y NX) (* z NX NY))))
          (setf-vec3-array *x* i (vec3 (+ (vec3-x init-min) d (* x d))
                                       (+ (vec3-y init-min) d (* y d))
                                       (+ (vec3-z init-min) d (* z d)))))))))

(define-function poly6-kernel ((vec3 x))
  (let ((r scalar (norm x)))
    (if (<= r (scalar h))
        (* (/ 315d0
              (* 64d0 (scalar pi) (expt (scalar h) 9)))
           (expt (- (expt (scalar h) 2) (expt r 2)) 3))
        0d0)))

(define-function grad-spiky-kernel ((vec3 x))
  (let ((r scalar (norm x)))
    (if (<= r (scalar h))
        (* (/ -45d0 (* (scalar pi) (expt (scalar h) 6)))
           (expt (- (scalar h) r) 2)   
           (/ x r))
        (vec3 0d0 0d0 0d0))))

(define-function rap-visc-kernel ((vec3 x))
  (let ((r scalar (norm x)))
    (if (<= r (scalar h))
        (* (/ 45d0 (* (scalar pi) (expt (scalar h) 6)))
           (- (scalar h) r))
        0d0)))

(defun update-density ()
  (declare (optimize (speed 3) (safety 0)))
  (for-scalar-array *rho* i
    (setf-scalar-array *rho* i 0d0)
    (for-neighbors *nbr* (*x* i) j
      (incf-scalar-array *rho* i
        (let ((dr vec3 (* (- (vec3-aref *x* i)
                             (vec3-aref *x* j))
                          (scalar simscale))))
          (* (scalar pmass) (poly6-kernel dr)))))))

(defun update-pressure ()
  (declare (optimize (speed 3) (safety 0)))
  (for-scalar-array *prs* i
    (setf-scalar-array *prs* i (* (- (scalar-aref *rho* i)
                                     (scalar restdensity))
                                  (scalar intstiff)))))

(define-function pressure-term ((int i) (int j))
  (let ((dr vec3 (* (- (vec3-aref *x* i) (vec3-aref *x* j))
                    (scalar simscale))))
    (* (* (- (scalar pmass)) (/ (+ (scalar-aref *prs* i) (scalar-aref *prs* j))
                                (* 2d0 (scalar-aref *rho* j))))
       (grad-spiky-kernel dr))))

(define-function viscosity-term ((int i) (int j))
  (let ((dr vec3 (* (- (vec3-aref *x* i) (vec3-aref *x* j))
                    (scalar simscale))))
    (* (* (scalar visc) (/ (* (scalar pmass)
                              (- (vec3-aref *v* j) (vec3-aref *v* i)))
                           (scalar-aref *rho* j)))
       (rap-visc-kernel dr))))

(defun update-force ()
  (declare (optimize (speed 3) (safety 0)))
  (for-vec3-array *f* i
    (setf-vec3-array *f* i (vec3 0d0 0d0 0d0))
    (for-neighbors *nbr* (*x* i) j
      (when (/= i j)
        (incf-vec3-array *f* i
          (+ (pressure-term (int i) (int j))
             (viscosity-term (int i) (int j))))))))

(define-function wall ((scalar d) (vec3 norm) (vec3 a))
  (let ((diff scalar (- (* 2d0 (scalar radius))
                        (* d (scalar simscale))))
        (adj scalar (- (* (scalar extstiff) diff)
                       (* (scalar extdamp) (dot norm (vec3-aref *v* i))))))
    (if (> diff (scalar epsilon))
        (+ a (* adj norm))
        a)))

(define-function x-wall-max ((int i) (vec3 a))
  (wall (- (vec3-x (vec3 box-max)) (vec3-x (vec3-aref *x* i)))
        (-1d0 0d0 0d0)
        a))

(define-function x-wall-min ((int i) (vec3 a))
  (wall (- (vec3-x (vec3-aref *x* i)) (vec3-x (vec3 box-min)))
        (1d0 0d0 0d0)
        a))

(define-function y-wall-max ((int i) (vec3 a))
  (wall (- (vec3-y (vec3 box-max)) (vec3-y (vec3-aref *x* i)))
        (0d0 -1d0 0d0)
        a))

(define-function y-wall-min ((int i) (vec3 a))
  (wall (- (vec3-y (vec3-aref *x* i)) (vec3-y (vec3 box-min)))
        (0d0 1d0 0d0)
        a))

(define-function z-wall-max ((int i) (vec3 a))
  (wall (- (vec3-z (vec3 box-max)) (vec3-z (vec3-aref *x* i)))
        (0d0 0d0 -1d0)
        a))

(define-function z-wall-min ((int i) (vec3 a))
  (wall (- (vec3-z (vec3-aref *x* i)) (vec3-z (vec3 box-min)))
        (0d0 0d0 1d0)
        a))

(define-function accel-limit ((vec3 a))
  (let ((speed scalar (norm a)))
    (if (> speed (scalar limit))
        (* (/ (scalar limit) speed) a)
        a)))

(define-function boundary ((int i) (vec3 a))
    (x-wall-min i
      (x-wall-max i
        (y-wall-min i
          (z-wall-max i
            (z-wall-min i
              (accel-limit a)))))))

(defun update-velocity ()
  (declare (optimize (speed 3) (safety 0)))
  (declare (type vec3 g box-min box-max))
  (for-vec3-array *v* i
    (incf-vec3-array *v* i
      (let ((a vec3 (+ (/ (vec3-aref *f* i) (scalar-aref *rho* i)) (vec3 g))))
        (* (boundary (int i) a) (scalar dt))))))

(defun update-position ()
  (declare (optimize (speed 3) (safety 0)))
  (for-vec3-array *x* i
    (incf-vec3-array *x* i (/ (* (vec3-aref *v* i) (scalar dt))
                              (scalar simscale)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defun main ()
  (print-number-of-particles)
  (initialize)
; (sb-sprof:with-profiling (:report :graph :loop nil)
    (dotimes (i 300)
      ; (output i)
      ; (print-step i)
      (update-neighbor-map *nbr* *x*)
      (update-density)
      (update-pressure)
      (update-force)
      (update-velocity)
      (update-position)));)