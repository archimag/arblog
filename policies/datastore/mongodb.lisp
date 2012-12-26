;;;; mongodb.lisp

(defpackage #:arblog.datastore.mongodb
  (:use #:cl #:iter #:son-sugar #:arblog.policy.datastore)
  (:export #:arblog-mongo-datastore))

(in-package #:arblog.datastore.mongodb)

(defclass arblog-mongo-datastore ()
  ((dbspec :initarg :dbspec :initform '(:name "blog") :reader dbspec)))

(defmacro with-posts-collection ((name st) &body body)
  (let ((blog-symbol (gensym)))
    `(let* ((,blog-symbol (apply 'make-instance 'mongo:database (dbspec ,st)))
            (,name (mongo:collection ,blog-symbol "posts")))
       (unwind-protect
            (progn ,@body)
         (mongo:close-database ,blog-symbol)))))

(defun calc-sha1-sum (val)
  "Calc sha1 sum of the val (string)"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1
                             (babel:string-to-octets val :encoding :utf-8))))

(defun list-fields-query (fields)
  (let ((fields-query nil))
    (when fields
      (setf fields-query (make-hash-table :test 'equal))
      (iter (for field in fields)
            (setf (gethash field fields-query) 1)))
    fields-query))

(defmethod datastore-count-posts ((datastore arblog-mongo-datastore) &optional tag)
  (with-posts-collection (posts datastore)
    (mongo:collection-count posts
                            (and tag (son "tags" tag)))))

(defmethod datastore-list-recent-posts ((datastore arblog-mongo-datastore) skip limit &key tag fields)
  (with-posts-collection (posts datastore)
    (mongo:find-list posts
                     :query (son "$query" (if tag
                                              (son "tags" tag)
                                              (son))
                                 "$orderby" (son "published" -1))
                     :limit limit
                     :skip skip
                     :fields (list-fields-query fields))))
  
(defmethod datastore-find-single-post ((datastore arblog-mongo-datastore) year month day urlname)
  (let* ((min (local-time:encode-timestamp 0 0 0 0 day month year))
         (max (local-time:adjust-timestamp min (offset :day 1))))
    (with-posts-collection (posts datastore)
      (mongo:find-one posts
                      (son "published"
                           (son "$gte" min "$lt" max)
                           "urlname" urlname)))))

(defmethod datastore-get-single-post ((datastore arblog-mongo-datastore) id &key fields)
  (with-posts-collection (posts datastore)
    (mongo:find-one posts
                    (son "_id" id)
                    (list-fields-query fields))))
  

(defmethod datastore-list-archive-posts ((datastore arblog-mongo-datastore) min max &optional fields)
  (let ((fields-query (list-fields-query fields)))
    (with-posts-collection (posts datastore)
      (mongo:find-list posts
                       :query (son "$query" (son "published"
                                                 (son "$gte" min "$lt" max))
                                   "$orderby" (son "published" -1))
                       :fields fields-query))))

(defmethod datastore-all-tags ((datastore arblog-mongo-datastore))
  (with-posts-collection (posts datastore)
    (mongo:with-cursor (cursor posts (son) (son "tags" 1))
      (let ((tags nil))
        (mongo:docursor (item cursor)
          (iter (for tag in (gethash "tags" item))
                (pushnew tag tags :test #'string=)))
        tags))))

(defmethod datastore-insert-post ((datastore arblog-mongo-datastore) title tags content &key markup published updated)
  (let* ((now (local-time:now))
         (id (calc-sha1-sum (format nil "~A~A" title published)))
         (post (son "_id" id
                    "title" title
                    "ulrname" (arblog:title-to-urlname title)
                    "published" now
                    "updated" now
                    "content" content
                    "tags" (coerce tags 'vector))))
    (when markup
      (setf (gethash "markup" post)
            markup))
    (when published
      (setf (gethash "published" post)
            published))
    (when updated
      (setf (gethash "published" post)
            updated))
    (with-posts-collection (posts datastore)
      (mongo:insert-op posts post))
    id))

(defmethod datastore-update-post ((datastore arblog-mongo-datastore) id title tags content &key markup)
  (with-posts-collection (posts datastore)
    (let ((post  (mongo:find-one posts (son "_id" id))))
      (setf (gethash "title" post) title
            (gethash "urlname" post) (arblog:title-to-urlname title)
            (gethash "content" post) content
            (gethash "tags" post) (coerce tags 'vector)
            (gethash "updated" post) (local-time:now)
            (gethash "markup" post) markup)
      (mongo:update-op posts (son "_id" id) post))))

(defmethod datastore-set-admin ((datastore arblog-mongo-datastore) admin-name admin-password)
  (destructuring-bind (&key name hostname port username password) (dbspec datastore)
    (mongo:with-database (db name :hostname hostname :port port :username username :password password)
      (mongo:update-op (mongo:collection db "meta")
                       (son "_id" "admin")
                       (son "_id" "admin"
                            "info" (son "name" admin-name
                                        "password" (calc-sha1-sum admin-password)))
                       :upsert t))))

(defmethod datastore-check-admin ((datastore arblog-mongo-datastore) admin-name admin-password)
  (destructuring-bind (&key name hostname port username password) (dbspec datastore)
    (mongo:with-database (db name :hostname hostname :port port :username username :password password)
      (let ((admin (gethash "info" (mongo:find-one (mongo:collection db "meta") (son "_id" "admin")))))
        (and (string= (gethash "name" admin) admin-name)
             (string= (gethash "password" admin) (calc-sha1-sum admin-password)))))))

;;;  Helpers 

(defun import-posts-from-datastore (origin target)
  (with-posts-collection (origin-posts origin)
    (with-posts-collection (target-posts target)
      (mongo:with-cursor (origin-post-cursor origin-posts (son))
        (mongo:docursor (post origin-post-cursor)
          (mongo:insert-op target-posts post))))))
        
(defun remove-all-posts (datastore)
  (with-posts-collection (posts datastore)
    (mongo:delete-op posts (son))))

;;; upgrade

(defun upgrade-datastore (datastore)
  (flet ((upgrade-post (post)
           (when (gethash "content-rst" post)
             (setf (gethash "markup" post)
                   (gethash "content-rst" post))
             (remhash "content-rst" post))
           (setf (gethash "urlname" post)
                 (arblog:title-to-urlname (gethash "title" post)))))
    (with-posts-collection (posts datastore)
      (mongo:with-cursor (cursor posts (son))
        (mongo:docursor (post cursor)
          (upgrade-post post)
          (mongo:update-op posts (son "_id" (gethash "_id" post)) post))))))
