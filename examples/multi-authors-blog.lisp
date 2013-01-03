;;;; demo.lisp

(asdf:operate 'asdf:load-op '#:arblog)
(asdf:operate 'asdf:load-op '#:arblog-systems)

(asdf:operate 'asdf:load-op '#:arblog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:arblog-markup-rst)
;; (asdf:operate 'asdf:load-op '#:arblog-markup-markdown)

(asdf:operate 'asdf:load-op '#:arblog-theme-mirev)

(restas:define-module #:my-multi-authors-blog
  (:use #:cl))

(in-package #:my-multi-authors-blog)

;;;; datastore

(defclass multi-authors-datastore (arblog.datastore.mongodb:arblog-mongo-datastore)
  ((author :initarg :author :reader blog-author))
  (:default-initargs
   :dbspec '(:name "multi-authors-blog")))

(defmethod arblog.datastore.mongodb:make-query ((datastore multi-authors-datastore) &rest args)
  (declare (ignore args))
  (let ((query (call-next-method)))
    (setf (gethash "author" query)
          (blog-author datastore))
    query))

(defun add-author (author password)
  (arblog.policy.datastore:datastore-set-admin
   (make-instance 'multi-authors-datastore :author author)
   author
   password))

(add-author "ivanov" "111")
(add-author "petrov" "222")

;;;; settings

(defun author-settings (author)
  (restas:make-context
   `((arblog:*blog-name* . ,author)
     (arblog.internal.datastore:*datastore* . ,(make-instance 'multi-authors-datastore :author author))
     (arblog.internal.markup:*markup* . ,(make-instance 'arblog.markup.rst:arblog-rst-markup))
     (arblog.internal.theme:*theme* . ,(make-instance 'arblog.theme.mirev:arblog-mirev-theme)))))

;;;; multi-authors-arblog-route

(defclass multi-authors-arblog-route (routes:proxy-route) ())

(defun @multi-authors (route)
  (make-instance 'multi-authors-arblog-route :target route))

(defmethod routes:route-template ((route multi-authors-arblog-route))
  (append (routes:parse-template ":author")
          (call-next-method)))

(defmethod restas:process-route :around ((route multi-authors-arblog-route) bindings)
  (let ((author (cdr (assoc :author bindings :test #'string=))))
    (restas:with-context (author-settings author)
      (call-next-method))))

(defmethod routes:route-check-conditions ((route multi-authors-arblog-route) bindings)
  (let ((author (cdr (assoc :author bindings :test #'string=))))
    (restas:with-context (author-settings author)
      (call-next-method))))
  

(defmethod restas:make-route-url ((route multi-authors-arblog-route) bindings)
  (restas:make-route-url (routes:route-template route)
                         (list* :author (blog-author arblog.internal.datastore:*datastore*)
                                bindings)))

;;;; mount modules

(restas:mount-module -public- (#:arblog.public)
  (:decorators '@multi-authors))

(restas:mount-module -admin- (#:arblog.admin)
  (:url "/admin/")
  (:decorators '@multi-authors 'arblog:@admin))

(restas:mount-module -static- (#:arblog.static)
  (:url "/static/"))

;;;; start

(restas:start '#:my-multi-authors-blog :port 8080)
