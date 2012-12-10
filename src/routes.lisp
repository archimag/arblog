;;;; routes.lisp

(in-package #:arblog)

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
  (let ((skip (or (ignore-errors (parse-integer (hunchentoot:get-parameter "skip"))) 0)))
    (list :list-posts-page
          :navigation (navigation (restas:genurl 'entry)
                                  skip
                                  (st.count-posts))
          :posts (st.list-recent-posts skip *posts-on-page*))))

;;;; One post

(restas:define-route one-post (":year/:month/:day/:title"
                               :parse-vars (list :year #'parse-integer
                                                 :month #'parse-integer
                                                 :day #'parse-integer))
  (list :one-post-page
        :post (st.find-single-post year month day title)))

(restas:define-route post-permalink ("permalink/posts/:id")
  (let* ((info (st.post-short-info id))
         (title (gethash "title" info))
         (published (gethash "published" info)))
    (restas:redirect 'one-post
                     :year (local-time:timestamp-year published)
                     :month (local-time:timestamp-month published)
                     :day (local-time:timestamp-day published)
                     :title title)))

;;;; archive
  
(restas:define-route archive-for-year (":year/"
                                        :parse-vars (list :year #'parse-integer))
  (let* ((min (local-time:encode-timestamp 0 0 0 0 1 1 year))
         (max (local-time:adjust-timestamp min (offset :year 1)))
         (posts (st.list-archive-posts min max '("published")))
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
          :posts (st.list-archive-posts min max))))

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
          :posts (st.list-archive-posts min max))))

;;;; Tags

(restas:define-route all-tags ("tags/")
  (list :tags-page
        :tags (st.all-tags)))
                 
(restas:define-route posts-with-tag ("tags/:tag")
  (let ((skip (or (ignore-errors (parse-integer (hunchentoot:get-parameter "skip"))) 0)))  
    (list :posts-with-tag-page
          :tag tag
          :navigation (navigation (restas:genurl 'posts-with-tag :tag tag)
                                  skip
                                  (st.count-posts tag))
          :posts (st.list-recent-posts skip
                                    *posts-on-page*
                                    tag))))

;;;; Feeds

(restas:define-route posts-feed ("feeds/atom"
                                 :content-type "application/atom+xml")
  (list :atom-feed
        :name "archimag"
        :href-atom (restas:gen-full-url 'posts-feed)
        :href-html (restas:gen-full-url 'entry)
        :posts (st.list-recent-posts 0 50)))

(restas:define-route posts-with-tag-feed ("feeds/atom/tag/:tag"
                                          :content-type "application/atom+xml")
  (list :atom-feed
        :name (format nil "archimag blog posts with tag \"~A\"" tag)
        :href-atom (restas:gen-full-url 'posts-feed)
        :href-html (restas:gen-full-url 'entry)
        :posts (st.list-recent-posts 0 50 tag)))
