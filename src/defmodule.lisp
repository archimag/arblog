;;;; defmodule.lisp

(restas:define-module #:arblog
  (:use #:cl #:iter #:son-sugar))

(in-package #:arblog)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *basepath*
    (make-pathname :directory (pathname-directory (asdf:component-pathname (asdf:find-system '#:arblog))))))

(closure-template:compile-template :common-lisp-backend
 (fad:list-directory (merge-pathnames "templates/" *basepath*)))


(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (unless (restas:context-symbol-value context '*default-render-method*)
    (restas:context-add-variable context
                                 '*default-render-method*
                                 (make-instance 'drawer))))


(defparameter *disqus-shortname* "archimagblog")

(defparameter *disqus-developer-mode* t)

(defparameter *posts-on-page* 10)