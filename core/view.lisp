;;;; render.lisp

(in-package #:arblog)

(defclass arblog-view () ())

(defmethod restas:render-object ((view arblog-view) (data list))
  (apply 'theme.render-tagged-data (car data) (cdr data)))

(setf *default-render-method* (make-instance 'arblog-view))

(defparameter *theme-static-dir-map* (make-hash-table :test 'equal))

(defun register-theme-static-dir (theme-name path)
  (setf (gethash theme-name *theme-static-dir-map*)
        path))
  
