#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-examples.nbody
  (:use :cl
        :mini-lang)
  (:export :main))
(in-package :mini-lang-examples.nbody)


(defvar dt 1.0d-1)
(defvar m 1.0d0)
(defvar G 6.67259d-11)

(defmacro do-double3-array ((var array) &body body)
  `(dotimes (,var (double3-array-dimensions ,array))
     (declare (type fixnum ,var))
     ,@body))

(defun initialize (xs vs)
  (declare (optimize (speed 3) (safety 0)))
  ;; Initialize position.
  (do-double3-array (i xs)
    (let ((x (random 1.0d0))
          (y (random 1.0d0))
          (z (random 1.0d0)))
      (setf (double3-aref* xs i) (values x y z))))
  ;; Initialize velocity.
  (do-double3-array (i vs)
    (setf (double3-aref* vs i)
          (eval-mini-lang
           (double3 0.0d0 0.0d0 0.0d0)))))

(defun update-xs (xs vs)
  (declare (optimize (speed 3) (safety 0)))
  (do-double3-array (i xs)
    (setf (double3-aref* xs i)
          (eval-mini-lang
           (+ (the double3 (aref xs i))
              (*. (aref vs i) dt))))))

(defun update-vs (vs as)
  (declare (optimize (speed 3) (safety 0)))
  (do-double3-array (i vs)
    (setf (double3-aref* vs i)
          (eval-mini-lang
           (+ (the double3 (aref vs i))
              (*. (aref as i) dt))))))

(defun update-as (as fs)
  (declare (optimize (speed 3) (safety 0)))
  (do-double3-array (i as)
    (setf (double3-aref* as i)
          (eval-mini-lang
           (/. (the double3 (aref fs i)) m)))))

(defun update-fs (fs xs)
  (declare (optimize (speed 3) (safety 0)))
  (do-double3-array (i fs)
    (setf (double3-aref* fs i)
          (eval-mini-lang
           (double3 0.0d0 0.0d0 0.0d0)))
    (do-double3-array (j xs)
      (when (/= i j)
        (setf (double3-aref* fs i)
              (eval-mini-lang
               (+ (aref fs i)
                  (let ((r (- (the double3 (aref xs j))
                              (aref xs i))))
                    (let ((|r| (norm r)))
                      (.* (/ (* m m G)
                             (* |r| |r|))
                          (/. r |r|)))))))))))

(defun main (n)
  (let ((xs (make-double3-array n))
        (vs (make-double3-array n))
        (as (make-double3-array n))
        (fs (make-double3-array n)))
    (initialize xs vs)
    (loop repeat 100
       do (update-fs fs xs)
          (update-as as fs)
          (update-vs vs as)
          (update-xs xs vs))))
