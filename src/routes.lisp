;;;; routes.lisp

(in-package #:arblog)

;;;; static files

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (merge-pathnames "static/" *basepath*)))

;;;; main page

(restas:define-route entry ("")
  (mongo:with-database (blog "blog")
    (list :list-posts-page
          :posts (mongo:find-list (mongo:collection blog "posts")
                                  :query (son "$query" (son) 
                                              "$orderby" (son "published" -1))
                                  :limit 10
                                  :skip (or (ignore-errors (parse-integer (hunchentoot:get-parameter "skip")))
                                            0)))))
  
(defun archive-post (min max &optional fields)
  (mongo:with-database (blog "blog")
    (mongo:find-list (mongo:collection blog "posts")
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

(restas:define-route one-post (":year/:month/:day/:title"
                               :parse-vars (list :year #'parse-integer
                                                 :month #'parse-integer
                                                 :day #'parse-integer))
  (let* ((min (local-time:encode-timestamp 0 0 0 0 day month year))
         (max (local-time:adjust-timestamp min (offset :day 1))))
    (mongo:with-database (blog "blog")  
      (list :one-post-page
            :post (mongo:find-one (mongo:collection blog "posts")
                                  (son "published"
                                       (son "$gte" min "$lt" max)
                                       "title" title))))))

(restas:define-route all-tags ("tags/")
  "Hello")

(restas:define-route posts-with-tag ("tags/:tag")
  tag)
  
  


        