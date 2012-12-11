;;;; defmodule.lisp

(restas:define-module #:arblog
  (:use #:cl #:iter #:son-sugar)
  (:import-from #:docutils.parser.rst #:&option #:&content #:&content-parser))

(in-package #:arblog)

(closure-template:compile-cl-templates
 (fad:list-directory (asdf:system-relative-pathname '#:arblog "templates/")))

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (unless (restas:context-symbol-value context '*default-render-method*)
    (restas:context-add-variable context
                                 '*default-render-method*
                                 (make-instance 'view)))
  (unless (restas:context-symbol-value context '*datastore*)
    (restas:context-add-variable context
                                 '*datastore*
                                 (make-instance 'arblog-mongo-datastore))))

(defparameter *disqus-enabled* nil)

(defparameter *disqus-shortname* "archimagblog")

(defparameter *disqus-developer-mode* t)

(defparameter *posts-on-page* 10)

(defvar *datastore* nil)

;;;; static files

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (asdf:system-relative-pathname '#:arblog "static/")))
