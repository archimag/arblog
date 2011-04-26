;;;; utils.lisp

(in-package #:arblog)

(defun calc-sha1-id (title published)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1 (babel:string-to-octets (format nil "~A~A" title published)
                                  :encoding :utf-8))))


(defun load-post-from-file (path title &rest tags
                            &aux (now (local-time:now)) (rst (alexandria:read-file-into-string path)))
  (let ((post (son "_id" (calc-sha1-id title now)
                   "title" title
                   "published" now
                   "updated" now
                   "content-rst" rst
                   "content" (render-arblog-markup rst)
                   "tags" (coerce tags 'vector))))
    (with-posts-collection posts
      (mongo:insert-op posts post))))

(defun delete-last-post ()
  (with-posts-collection posts
    (mongo:delete-op posts
                     (mongo:find-one posts
                                     (son "$query" (son) 
                                          "$orderby" (son "published" -1))
                                     (son "_id" 1)))))
                  
                  
(defun copy-last-post-to (db)
  (let ((post (with-posts-collection posts
                       (mongo:find-one posts
                                       (son "$query" (son) 
                                            "$orderby" (son "published" -1))))))
    (setf (gethash "tags" post)
          (coerce (gethash "tags" post) 'vector))
    (mongo:insert-op (mongo:collection db "posts")
                     post)))