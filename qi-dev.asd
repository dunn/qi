#|
  This file is a part of qi project.
  Copyright (c) 2015 Cody Reichert (codyreichert@gmail.com)
|#

(in-package :cl-user)
(defpackage qi-dev-asd
  (:use :cl :asdf))
(in-package :qi-dev-asd)

(defsystem qi-dev
  :author "Cody Reichert"
  :license ""
  :depends-on (:qi :cl-fad)
  :components ((:module "dev"
                :components ((:file "import"))))
  :description "Development tools for qi")
