;;;; datastore.lisp

(in-package #:arblog)

;;;; Interface

(defmacro define-datastore-method (name (&body args) &optional documentation)
  (let ((datastore-generic-method (intern (format nil "DATASTORE-~A" name)))
        (internal-function (intern (format nil "ST.~A" name)))
        (args-for-call (iter (for item in args)
                             (with key-args-p = nil)
                             (unless (member item '(&optional &key))
                               (when key-args-p
                                 (collect (intern (symbol-name item) :keyword)))
                               (collect item))
                             (when (eql item '&key)
                               (setf key-args-p t)))))
  `(progn
     (defgeneric ,datastore-generic-method (datastore ,@args)
       ,@(and documentation (list (list :documentation documentation))))
     (defun ,internal-function (,@args)
       ,@(and documentation (list documentation))
       (,datastore-generic-method *datastore* ,@args-for-call)))))

(define-datastore-method count-posts (&optional tag)
  "Return a count of the posts that are published")

(define-datastore-method list-recent-posts (skip limit &key tag fields)
  "Retrieve the recent posts.")

(define-datastore-method find-single-post (year month day title)
  "Retrieve a single post, based on date and post title")

(define-datastore-method get-single-post (id &key fields)
  "Retrieve a single post, based  on post ID")

(define-datastore-method list-archive-posts (min max &optional fields)
  "Retrieve archive posts")

(define-datastore-method all-tags ()
  "Retrieve an array of tags")

(define-datastore-method insert-post (title tags content &key content-rst published updated)
  "Insert post in the datastore and return the post ID of the created post")

(define-datastore-method update-post (id title tags content &key content-rst)
  "Update post in the datastore")

(define-datastore-method set-admin (name password)
  "Set administrator name and password")

(define-datastore-method check-admin (name password)
  "Check for administrator rights")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arblog-mongo-datastore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  
(defmethod datastore-find-single-post ((datastore arblog-mongo-datastore) year month day title)
  (let* ((min (local-time:encode-timestamp 0 0 0 0 day month year))
         (max (local-time:adjust-timestamp min (offset :day 1))))
    (with-posts-collection (posts datastore)
      (mongo:find-one posts
                      (son "published"
                           (son "$gte" min "$lt" max)
                           "title" title)))))

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

(defmethod datastore-insert-post ((datastore arblog-mongo-datastore) title tags content &key content-rst published updated)
  (let* ((now (local-time:now))
         (id (calc-sha1-sum (format nil "~A~A" title published)))
         (post (son "_id" id
                    "title" title
                    "published" now
                    "updated" now
                    "content" content
                    "tags" (coerce tags 'vector))))
    (when content-rst
      (setf (gethash "content-rst" post)
            content-rst))
    (when published
      (setf (gethash "published" post)
            published))
    (when updated
      (setf (gethash "published" post)
            updated))
    (with-posts-collection (posts datastore)
      (mongo:insert-op posts post))
    id))

(defmethod datastore-update-post ((datastore arblog-mongo-datastore) id title tags content &key content-rst)
  (with-posts-collection (posts datastore)
    (let ((post  (mongo:find-one posts (son "_id" id))))
      (setf (gethash "title" post) title
            (gethash "content" post) content
            (gethash "tags" post) (coerce tags 'vector)
            (gethash "updated" post) (local-time:now)
            (gethash "content-rst" post) content-rst)
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
