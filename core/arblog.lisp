;;;; routes.lisp

(in-package #:arblog)

(defparameter *disqus-enabled* nil)
(defparameter *disqus-shortname* nil)
(defparameter *disqus-developer-mode* t)

(defparameter *posts-on-page* 10)

(defparameter *blog-name* "blog")

(defun parse-skip-param ()
  (ignore-errors (parse-integer (hunchentoot:get-parameter "skip"))))

(defun url-with-skip (url skip)
  (let ((parsed-url (puri:parse-uri url)))
    (setf (puri:uri-query parsed-url)
          (format nil "skip=~A" skip))
    (puri:render-uri parsed-url nil)))

(defun navigation (url skip total-count)
  (list :older (if (< (+ skip *posts-on-page*) total-count)
                   (url-with-skip url
                                  (+ skip *posts-on-page*)))
        :newer (cond
                 ((= skip 0) nil)
                 ((> (- skip *posts-on-page*) 0)
                  (url-with-skip url (- skip *posts-on-page*)))
                 (t url))))

(defparameter *theme-static-dir-map* (make-hash-table :test 'equal))

(defun register-theme-static-dir (theme-name path)
  (setf (gethash theme-name *theme-static-dir-map*)
        path))

(defun title-to-urlname (title)
  (coerce (iter (for ch in-string title)
                (collect (if (char= ch #\Space) #\_ ch)))
          'string))

(defclass admin-route (routes:proxy-route) ())

(defmethod restas:process-route :before ((route admin-route) bindings)
  (multiple-value-bind (user password) (hunchentoot:authorization)
    (unless (arblog.internal.datastore:ds.check-admin user password)
      (hunchentoot:require-authorization *blog-name*))))

(defun @admin (route)
  (make-instance 'admin-route :target route))


;; mount

(restas:mount-module -public- (#:arblog.public)
  (:inherit-parent-context t))

(restas:mount-module -admin- (#:arblog.admin)
  (:inherit-parent-context t)
  (:url "/admin/")
  (:decorators '@admin)

  (arblog.admin:*post-permalink-route* '-public-.post-permalink))

(restas:mount-module -static- (#:arblog.static)
  (:url "/static/"))
