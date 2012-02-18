#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-example.diffuse
  (:use :cl
        :mini-lang)
  (:export :main))
(in-package :mini-lang-example.diffuse)

(defvar *f*)
(defvar *fn*)
(defvar *NX* 256)
(defvar *NY* 256)

(defun allocate (nx ny)
  (let ((n (* nx ny)))
    (setf *f* (make-scalar-array n))
    (setf *fn* (make-scalar-array n))))

(declaim (inline index))
(defun index (nx jx jy)
  (the fixnum (+ (the fixnum (* nx jy)) jx)))

(defun initialize (nx ny dx dy)
  (let ((alpha 30d0))
    (dotimes (jy ny)
      (dotimes (jx nx)
        (let ((j (index nx jx jy))
              (x (- (* dx (+ (float jx 1d0) 0.5d0)) 0.5d0))
              (y (- (* dy (+ (float jy 1d0) 0.5d0)) 0.5d0)))
          (setf-scalar (scalar-aref *f* j)
                       (exp (* (- (scalar alpha))
                               (+ (* (scalar x) (scalar x))
                                  (* (scalar y) (scalar y)))))))))))

(defun diffusion2d (nx ny kappa dt dx dy)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type double-float kappa dt dx dy)
           (type fixnum nx ny))
  (let* ((c0 (* kappa (/ dt (* dx dx))))
         (c1 (* kappa (/ dt (* dy dy))))
         (c2 (- 1d0 (* 2d0 (+ c0 c1)))))
    (dotimes (jy ny)
      (dotimes (jx nx)
        (let ((j (index nx jx jy)))
          (setf-scalar (scalar-aref *fn* j)
                       (let ((fcc scalar (scalar-aref *f* j))
                             (fcw scalar (if (= (int jx) 0)
                                             fcc
                                             (scalar-aref *f* (- j 1))))
                             (fce scalar (if (= (int jx) (- (int nx) 1))
                                             fcc
                                             (scalar-aref *f* (+ j 1))))
                             (fcs scalar (if (= (int jy) 0)
                                             fcc
                                             (scalar-aref *f* (- j nx))))
                             (fcn scalar (if (= (int jy) (- (int ny) 1))
                                             fcc
                                             (scalar-aref *f* (+ j nx)))))
                         (+ (* (scalar c0) (+ fce fcw))
                            (* (scalar c1) (+ fcn fcs))
                            (* (scalar c2) fcc)))))))))

(defun image-value (i j nx fmax fmin)
  (let ((fc (scalar-aref *f* (index nx i j))))
    (truncate (* 256.0
                 (/ (- fc fmin) (- fmax fmin))))))

(defun file-name (dir i nout)
  (let ((n (truncate (/ i nout))))
    (concatenate 'string dir (format nil "~4,'0D.pgm" n))))

(defun output-pnm (dir i nout nx ny)
  (let ((image (make-instance 'imago:grayscale-image
                              :width nx :height ny)))
    (dotimes (i nx)
      (dotimes (j ny)
        (setf (imago:image-pixel image i j) (image-value i j nx 1.0 0.0))))
    (imago:write-pnm image (file-name dir i nout) :ASCII)))

(defmacro swap (a b)
  `(rotatef ,a ,b))

(defun main ()
  (let* ((nx *NX*)
         (ny *NY*)
         (Lx 1d0)
         (Ly 1d0)
         (dx (/ Lx (float nx 1d0)))
         (dy (/ Ly (float ny 1d0)))
         (kappa 0.1d0)
         (dt (* 0.2d0 (/ (min (* dx dx) (* dy dy)) kappa)))
         (nout 500)
         (dir (namestring (truename "./"))))
    (allocate nx ny)
    (initialize nx ny dx dy)
    (dotimes (i 20000)
      (diffusion2d nx ny kappa dt dx dy)
      (when (eq (mod i nout) 0)
        (output-pnm dir i nout nx ny))
      (swap *f* *fn*))))
