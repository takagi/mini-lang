#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang.lang.symbol
  (:use :cl)
  (:export :mini-lang-symbol
           :mini-lang-symbol-p))
(in-package :mini-lang.lang.symbol)


;;
;; Symbol   

(deftype mini-lang-symbol ()
  '(satisfies mini-lang-symbol-p))

(defun mini-lang-symbol-p (object)
  (symbolp object))
