#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-examples-asd
  (:use :cl :asdf))
(in-package :mini-lang-examples-asd)

(defsystem mini-lang-examples
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:mini-lang :imago)
  :components ((:module "examples"
                :components
                ((:file "nbody")
                 (:file "diffuse")
                 (:file "sph"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
