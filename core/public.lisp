;;;; public.lisp

(in-package #:arblog.public)

(defmethod data-sift:compile-rule ((rule (eql 'year)) &key)
  (data-sift:compile-rule 'integer))

(defmethod data-sift:compile-rule ((rule (eql 'month)) &key)
  (data-sift:compile-rule '(integer :min-value 1 :max-value 12)))

(defmethod data-sift:compile-rule ((rule (eql 'day)) &key)
  (data-sift:compile-rule '(integer :min-value 1 :max-value 31)))

;;;; main page

(restas:define-route entry ("")
  (:apply-render-method #'render.list-recent-posts)
  (:additional-variables (skip (arblog::parse-skip-param) 0))
  (list (ds.list-recent-posts skip *posts-on-page*)
        (arblog::navigation (restas:genurl 'entry)
                    skip
                    (ds.count-posts))))
;;;; one post

(restas:define-route one-post (":year/:month/:day/:urlname")
  (:sift-variables (year 'year) (month 'month) (day 'day))
  (:render-method #'render.one-post)
  (ds.find-single-post year month day urlname))

(restas:define-route post-permalink ("permalink/posts/:id")
  (let* ((info (ds.get-single-post id :fields '("published" "urlname")))
         (urlname (gethash "urlname" info))
         (published (gethash "published" info)))
    (restas:redirect 'one-post
                     :year (local-time:timestamp-year published)
                     :month (format nil "~2,'0D" (local-time:timestamp-month published))
                     :day (format nil "~2,'0D" (local-time:timestamp-day published))
                     :urlname urlname)))

;; ;;;; archive

(restas:define-route archive-for-year (":year/" )
  (:sift-variables (year 'year))
  (:apply-render-method #'render.archive-for-year)
  (let* ((min (local-time:encode-timestamp 0 0 0 0 1 1 year))
         (max (local-time:adjust-timestamp min (offset :year 1)))
         (posts (ds.list-archive-posts min max '("published")))
         (months (make-hash-table)))
    (dolist (post posts)
      (setf (gethash (local-time:timestamp-month (gethash "published" post)) months)
            1))
    (list year
          (sort (iter (for (month x) in-hashtable months)
                      (collect month))
                #'<))))

(restas:define-route archive-for-month (":year/:month/")
  (:sift-variables (year 'year) (month 'month))
  (:apply-render-method #'render.archive-for-month)
  (let* ((min (local-time:encode-timestamp 0 0 0 0 1 month year))
         (max (local-time:adjust-timestamp min (offset :month 1))))
    (list year month (ds.list-archive-posts min max))))

(restas:define-route archive-for-day (":year/:month/:day/")
  (:sift-variables (year 'year) (month 'month) (day 'day))
  (:apply-render-method #'render.archive-for-day)
  (let* ((min (local-time:encode-timestamp 0 0 0 0 day month year))
         (max (local-time:adjust-timestamp min (offset :day 1))))
    (list year month day (ds.list-archive-posts min max))))

;;;; Tags

(restas:define-route all-tags ("tags/")
  (:render-method #'render.all-tags)
  (ds.all-tags))

(restas:define-route posts-with-tag ("tags/:tag")
  (:apply-render-method #'render.posts-with-tag)
  (:additional-variables (skip (parse-skip-param) 0))
  (list tag
        (ds.list-recent-posts skip *posts-on-page* :tag tag)
        (navigation (restas:genurl 'posts-with-tag :tag tag)
                    skip
                    (ds.count-posts tag))))

;;;; Feeds

(defun feed-post-info (post)
  (list :id (gethash "_id" post)
        :title (gethash "title" post)
        :link (restas:genurl* 'post-permalink :id (gethash "_id" post))
        :published (local-time:format-timestring nil (gethash "published" post))
        :updated (local-time:format-timestring nil (gethash "updated" post))
        :content (gethash "content" post)))  

(restas:define-route posts-feed ("feeds/atom" :content-type "application/atom+xml")
  (:render-method #'arblog.feed.tmpl:atom-feed)
  (list :name *blog-name*
        :href-atom (restas:genurl* 'posts-feed)
        :href-html (restas:genurl* 'entry)
        :posts (mapcar #'feed-post-info (ds.list-recent-posts 0 50))))

(restas:define-route posts-with-tag-feed ("feeds/atom/tag/:tag" :content-type "application/atom+xml")
  (:render-method #'arblog.feed.tmpl:atom-feed)
  (list :name (format nil "~A blog posts with tag \"~A\"" *blog-name* tag)
        :href-atom (restas:genurl* 'posts-feed)
        :href-html (restas:genurl* 'entry)
        :posts (mapcar #'feed-post-info (ds.list-recent-posts 0 50))))
