(in-package :cl-user)
(defpackage qi.paths
  (:use :cl :qi.util)
  (:export :+dep-cache+
           :+project-name+
           :+qi-directory+
           :package-dir
           :project-dir
           :qi-dir))
(in-package :qi.paths)

;; Code:

;; Global paths

(defvar +qi-directory+ (asdf:system-source-directory "qi")
  "Pathname for the global Qi directory.")


(defun +dep-cache+ ()
  (uiop:merge-pathnames*
   #P"qi/archives/"
   (or (uiop:getenv "TMPDIR")
       "/tmp")))

;; Project local paths

(defvar +project-name+ nil
  "Name of the current, local project (from qi.yaml).  Returns NIL if
Qi is not running in the context of a project.")

(defun project-dir (proj)
  "Pathname/directory for <proj>."
  (asdf:system-source-directory proj))

(defun qi-dir ()
  (ensure-directories-exist
   (uiop:merge-pathnames*
    #P".dependencies/"
    (project-dir +project-name+))))

(defun package-dir ()
  (ensure-directories-exist
   (uiop:merge-pathnames*
    #P".dependencies/packages/"
    (project-dir +project-name+))))
