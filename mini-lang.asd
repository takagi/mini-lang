#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-asd
  (:use :cl :asdf))
(in-package :mini-lang-asd)

(defsystem mini-lang
  :version "0.1"
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:cl-tuples
               :cl-pattern
               :cl-unification)
  :components ((:module "src"
                :serial t
                :components
                ((:module "lang"
                  :serial t
                  :components
                  ((:file "data")
                   (:file "symbol")
                   (:file "type")
                   (:file "built-in")
                   (:file "syntax")
                   (:file "unification-environment")
                   (:file "type-environment")
                   (:file "variable-environment")
                   (:file "infer")
                   (:file "compile")
                   (:file "free-variables")
                   (:file "lang")))
                 (:file "mini-lang2"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op mini-lang-test))))
