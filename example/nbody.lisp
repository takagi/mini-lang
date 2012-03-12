#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-example.nbody
  (:use :cl
        :mini-lang)
  (:export :main))
(in-package :mini-lang-example.nbody)

(defvar *x*)
(defvar *v*)
(defvar *a*)
(defvar *f*)
(defvar dt  1d-2)
(defvar m   1d0)
(defvar g   9.8d0)

(defun initialize-x (n)
  (setf *x* (make-vec3-array n))
  (for-vec3-array *x* i
    (let* ((k (float i 1d0)))
      (setf-vec3-array *x* i (vec3 k k k)))))

(defun initialize-v (n)
  (setf *v* (make-vec3-array n)))

(defun initialize-a (n)
  (setf *a* (make-vec3-array n)))

(defun initialize-f (n)
  (setf *f* (make-vec3-array n)))

(defun initialize (n)
  (initialize-x n)
  (initialize-v n)
  (initialize-a n)
  (initialize-f n))

(defun update-x ()
  (for-vec3-array *x* i
    (incf-vec3-array *x* i (* (vec3-aref *v* i) (scalar dt)))))

(defun update-v ()
  (for-vec3-array *v* i
    (incf-vec3-array *v* i (* (vec3-aref *a* i) (scalar dt)))))

(defun update-a ()
  (for-vec3-array *a* i
    (setf-vec3-array *a* i (/ (vec3-aref *f* i) (scalar m)))))

(defun update-f ()  
  (declare (optimize (speed 3) (safety 0)))
  (let ((n (vec3-array-size *x*)))
    (for-vec3-array *f* i
      (setf-vec3-array *f* i (0d0 0d0 0d0))
      (dotimes (j n)
        (if (/= i j)
          (incf-vec3-array *f* i (let ((r (- (vec3-aref *x* j)
                                             (vec3-aref *x* i))) ; vec3
                                       (n (/ r (norm r)))) ; vec3
                                   (* (/ (* (scalar m) (scalar m) (scalar g))
                                         (* (norm r) (norm r)))
                                      n))))))))

(defun main (n)
  (initialize n)
  (dotimes (_ 100)
    (update-f)
    (update-a)
    (update-v)
    (update-x)))
