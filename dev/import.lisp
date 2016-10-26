#|
  This file is a part of qi project.
  Copyright (c) 2015 Cody Reichert (codyreichert@gmail.com)
|#

(in-package :cl-user)
(defpackage qi-dev
  (:use :cl))
(in-package :qi-dev)

(defun import-ql ()
  "Walk the quicklisp-projects directory and import all of those to qi format."
  (fad:walk-directory #p"/home/cody/workspace/projects/qi/manifest/projects/"
                      #'(lambda (x)
                          (let ((name (last
                                       (pathname-directory
                                        (uiop:pathname-directory-pathname x)))))
                            (with-open-file (s x)
                              (do ((line (read-line s ())
                                         (read-line s ())))
                                  ((null line))
                                (let ((words (nth-value
                                              1 (ppcre:scan-to-strings "^(.*?) (.*)" line))))
                                  (when words
                                    (setf *qi-packages*
                                          (pushnew
                                           (make-manifest-package
                                            :name (car name)
                                            :vc (svref words 0)
                                            :locations (pairlis (list "latest")
                                                                (list (svref words 1))))
                                           *qi-packages*))))))) t))
  (with-open-file (s +manifest-file+
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format s "~S" *qi-packages*)))
