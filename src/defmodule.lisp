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
                                 (make-instance 'view))))


(defparameter *disqus-shortname* "archimagblog")

(defparameter *disqus-developer-mode* nil)

(defparameter *posts-on-page* 10)

(defparameter *dbspec* '(:name "blog"))
