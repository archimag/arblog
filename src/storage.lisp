;;; datastore.lisp

(in-package #:arblog)

;;;; Interface

(defmacro define-datastore-method (name (&body args) &optional documentation)
  (let ((datastore-generic-method (intern (format nil "DATASTORE-~A" name)))
        (internal-function (intern (format nil "ST.~A" name)))
        (args-for-call (iter (for item in args)
                             (unless (member item '(&optional &key &rest &body &aux))
                               (collect item)))))
  `(progn
     (defgeneric ,datastore-generic-method (datastore ,@args)
       ,@(and documentation (list '(:documentation documenation))))
     (defun ,internal-function (,@args)
       ,@(and documentation (list documentation))
       (,datastore-generic-method *datastore* ,@args-for-call)))))

(define-datastore-method count-posts (&optional tag)
  "Return a count of the posts that are published")

(define-datastore-method list-recent-posts (skip limit &optional tag)
  "Retrieve the recent posts.")

(define-datastore-method find-single-post (year month day title)
  "Retrieve a single post, based on date and post title")

(define-datastore-method post-short-info (id)
  "Retrieve a single post shot info, based  on post ID")

(define-datastore-method list-archive-posts (min max &optional fields)
  "Retrieve archive posts")

(define-datastore-method all-tags ()
  "Retrieve an array of tags")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass arblog-mongo-datastore ()
  ((dbspec :initarg dbspec :initform '(:name "blog") :reader dbspec)))

(defmacro with-posts-collection ((name st) &body body)
  (let ((blog-symbol (gensym)))
    `(let* ((,blog-symbol (apply 'make-instance 'mongo:database (dbspec ,st)))
            (,name (mongo:collection ,blog-symbol "posts")))
       (unwind-protect
            (progn ,@body)
         (mongo:close-database ,blog-symbol)))))

;; post list

(defmethod datastore-count-posts ((datastore arblog-mongo-datastore) &optional tag)
  (with-posts-collection (posts datastore)
    (mongo:collection-count posts
                            (and tag (son "tags" tag)))))

(defmethod datastore-list-recent-posts ((datastore arblog-mongo-datastore) skip limit &optional tag)
  (with-posts-collection (posts datastore)
    (mongo:find-list posts
                     :query (son "$query" (if tag
                                              (son "tags" tag)
                                              (son))
                                 "$orderby" (son "published" -1))
                     :limit limit
                     :skip skip)))
  
;; one post

(defmethod datastore-find-single-post ((datastore arblog-mongo-datastore) year month day title)
  (let* ((min (local-time:encode-timestamp 0 0 0 0 day month year))
         (max (local-time:adjust-timestamp min (offset :day 1))))
    (with-posts-collection (posts datastore)
      (mongo:find-one posts
                      (son "published"
                           (son "$gte" min "$lt" max)
                           "title" title)))))
  
(defmethod datastore-post-short-info ((datastore arblog-mongo-datastore) id)
  (with-posts-collection (posts datastore)
    (mongo:find-one posts
                    (son "_id" id)
                    (son "published" 1
                         "title" 1))))

;; archive

(defmethod datastore-list-archive-posts ((datastore arblog-mongo-datastore) min max &optional fields)
  (let ((fields-query nil))
    (when fields
      (setf fields-query (make-hash-table :test 'equal))
      (iter (for field in fields)
            (setf (gethash field fields-query) 1)))
    (with-posts-collection (posts datastore)
      (mongo:find-list posts
                       :query (son "$query" (son "published"
                                                 (son "$gte" min "$lt" max))
                                   "$orderby" (son "published" -1))
                       :fields fields-query))))

;; tags

(defmethod datastore-all-tags ((datastore arblog-mongo-datastore))
  (with-posts-collection (posts datastore)
    (mongo:with-cursor (cursor posts (son) (son "tags" 1))
      (let ((tags nil))
        (mongo:docursor (item cursor)
          (iter (for tag in (gethash "tags" item))
                (pushnew tag tags :test #'string=)))
        tags))))
