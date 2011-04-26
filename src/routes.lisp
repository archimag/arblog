;;;; routes.lisp

(in-package #:arblog)

;;;; static files

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (merge-pathnames "static/" *basepath*)))

(defmacro with-posts-collection (name &body body)
  (let ((blog-symbol (gensym)))
    `(let* ((,blog-symbol (apply 'make-instance 'mongo:database *dbspec*))
            (,name (mongo:collection ,blog-symbol "posts")))
       (unwind-protect
            (progn ,@body)
         (mongo:close-database ,blog-symbol)))))

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

;;;; main page

(restas:define-route entry ("")
  (with-posts-collection posts
    (let ((skip (or (ignore-errors (parse-integer (hunchentoot:get-parameter "skip"))) 0)))
      (list :list-posts-page
            :navigation (navigation (restas:genurl 'entry)
                                    skip
                                    (mongo:collection-count posts))
            :posts (mongo:find-list posts
                                    :query (son "$query" (son) 
                                                "$orderby" (son "published" -1))
                                    :limit *posts-on-page*
                                    :skip skip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; One post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route one-post (":year/:month/:day/:title"
                               :parse-vars (list :year #'parse-integer
                                                 :month #'parse-integer
                                                 :day #'parse-integer))
  (let* ((min (local-time:encode-timestamp 0 0 0 0 day month year))
         (max (local-time:adjust-timestamp min (offset :day 1))))
    (with-posts-collection posts
      (list :one-post-page
            :post (mongo:find-one posts
                                  (son "published"
                                       (son "$gte" min "$lt" max)
                                       "title" title))))))

(restas:define-route post-permalink ("permalink/posts/:id")
  (let* ((info (with-posts-collection posts
                (mongo:find-one posts
                                (son "_id" id)
                                (son "published" 1
                                           "title" 1))))
         (title (gethash "title" info))
         (published (gethash "published" info)))
    (restas:redirect 'one-post
                     :year (local-time:timestamp-year published)
                     :month (local-time:timestamp-month published)
                     :day (local-time:timestamp-day published)
                     :title title)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; archive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun archive-post (min max &optional fields)
  (with-posts-collection posts
    (mongo:find-list posts
                     :query (son "$query" (son "published"
                                               (son "$gte" min "$lt" max))
                                 "$orderby" (son "published" -1))
                     :fields fields)))

(restas:define-route archive-for-year (":year/"
                                        :parse-vars (list :year #'parse-integer))
  (let* ((min (local-time:encode-timestamp 0 0 0 0 1 1 year))
         (max (local-time:adjust-timestamp min (offset :year 1)))
         (posts (archive-post min max (son "published" 1)))
         (months (make-hash-table)))
    (dolist (post posts)
      (setf (gethash (local-time:timestamp-month (gethash "published" post)) months)
            1))
    (list :archive-for-year
          :year year
          :months (sort (iter (for (month x) in-hashtable months)
                              (collect month))
                        #'<))))
    
(restas:define-route archive-for-month (":year/:month/"
                                        :parse-vars (list :year #'parse-integer
                                                          :month #'parse-integer))
  (let* ((min (local-time:encode-timestamp 0 0 0 0 1 month year))
         (max (local-time:adjust-timestamp min (offset :month 1))))
    (list :archive-for-month
          :year year
          :month month
          :posts (archive-post min max))))

(restas:define-route archive-for-day (":year/:month/:day/"
                                        :parse-vars (list :year #'parse-integer
                                                          :month #'parse-integer
                                                          :day #'parse-integer))
  (let* ((min (local-time:encode-timestamp 0 0 0 0 day month year))
         (max (local-time:adjust-timestamp min (offset :day 1))))
    (list :archive-for-day
          :year year
          :month month
          :day day
          :posts (archive-post min max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(restas:define-route all-tags ("tags/")
  (with-posts-collection posts
    (mongo:with-cursor (cursor posts (son) (son "tags" 1))
      (let ((tags nil))
        (mongo:docursor (item cursor)
          (iter (for tag in (gethash "tags" item))
                (pushnew tag tags :test #'string=)))
        (list :tags-page
              :tags tags)))))
                 
(restas:define-route posts-with-tag ("tags/:tag")
  (let ((skip (or (ignore-errors (parse-integer (hunchentoot:get-parameter "skip"))) 0)))  
    (with-posts-collection posts
      (list :posts-with-tag-page
            :tag tag
            :navigation (navigation (restas:genurl 'posts-with-tag :tag tag)
                                    skip
                                    (mongo:collection-count posts
                                                            (son "tags" tag)))
            :posts (mongo:find-list posts
                                    :query (son "$query" (son "tags" tag)
                                                "$orderby" (son "published" -1))
                                    :limit *posts-on-page*
                                    :skip (or (ignore-errors (parse-integer (hunchentoot:get-parameter "skip")))
                                              0))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Feeds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:define-route posts-feed ("feeds/atom"
                                 :content-type "application/atom+xml")
  (with-posts-collection posts
    (list :atom-feed
          :name "archimag"
          :href-atom (restas:gen-full-url 'posts-feed)
          :href-html (restas:gen-full-url 'entry)
          :posts (mongo:find-list posts
                                  :query (son "$query" (son) 
                                              "$orderby" (son "published" -1))
                                  :limit 50))))

(restas:define-route posts-with-tag-feed ("feeds/atom/tag/:tag"
                                          :content-type "application/atom+xml")
  (with-posts-collection posts
    (list :atom-feed
          :name (format nil "archimag blog posts with tag \"~A\"" tag)
          :href-atom (restas:gen-full-url 'posts-feed)
          :href-html (restas:gen-full-url 'entry)
          :posts (mongo:find-list posts
                                  :query (son "$query" (son "tags" tag) 
                                              "$orderby" (son "published" -1))
                                  :limit 50))))
