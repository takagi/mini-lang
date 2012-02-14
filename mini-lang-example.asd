#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang-example-asd
  (:use :cl :asdf))
(in-package :mini-lang-example-asd)

(defsystem mini-lang-example
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:mini-lang :imago)
  :components ((:module "example"
                :components
                ((:file "nbody")
                 (:file "diffuse")
                 (:file "sph"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
