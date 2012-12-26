;;;; render.lisp

(in-package #:arblog)

(defclass arblog-view () ())

(defmethod restas:render-object ((view arblog-view) (data list))
  (apply 'theme.render-tagged-data (car data) (cdr data)))

(setf *default-render-method* (make-instance 'arblog-view))
