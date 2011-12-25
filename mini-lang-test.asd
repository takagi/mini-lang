#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-test-asd
  (:use :cl :asdf))
(in-package :mini-lang-test-asd)

(defsystem mini-lang-test
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:mini-lang
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "mini-lang"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
