#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-example
  (:use :cl
        :mini-lang)
  (:export :main))
(in-package :mini-lang-example)

(define-vec3-array *x* *v* *a* *f*)
(defvar dt  1d-2)
(defvar m   1d0)
(defvar g   9.8d0)

(defun initialize-x (n)
  (setf *x* (make-vec3-array n))
  (for-vec3-array *x* i
    (let* ((k (float i 1d0))
           (x (make-vec3 k k k)))
      (setf-vec3-array (vec3 x)))))

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
    (incf-vec3-array (* (vec3-aref *v* i) (scalar dt)))))

(defun update-v ()
  (for-vec3-array *v* i
    (incf-vec3-array (* (vec3-aref *a* i) (scalar dt)))))

(defun update-a ()
  (for-vec3-array *a* i
    (setf-vec3-array (/ (vec3-aref *f* i) (scalar m)))))

(defun update-f ()  
  (declare (optimize (speed 3) (safety 0)))
  (let ((n (vec3-array-size *x*)))
    (for-vec3-array *f* i
      (setf-vec3-array (0d0 0d0 0d0))
      (dotimes (j n)
        (if (/= i j)
          (incf-vec3-array (let ((r vec3 (- (vec3-aref *x* j)
                                            (vec3-aref *x* i)))
                                 (n vec3 (/ r (norm r))))
                             (* (/ (* (scalar m) (scalar m) (scalar g))
                                   (* (norm r) (norm r)))
                                n))))))))

(defun update()
  (update-f)
  (update-a)
  (update-v)
  (update-x))

(defun main (n)
  (initialize n)
  (dotimes (_ 100)
    (update)))
