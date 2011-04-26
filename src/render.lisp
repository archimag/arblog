;;;; render.lisp

(in-package #:arblog)

(defclass drawer () ())

(defgeneric render-tagged-data (pagetype &rest args &key &allow-other-keys))

(defmethod restas:render-object ((drawer drawer) (data list))
  (apply 'render-tagged-data (car data) (cdr data)))

(defun archive-for-year-link (year)
  (list :title year
        :href (restas:genurl 'archive-for-year
                             :year year)))

(defun archive-for-month-link (year month)
  (list :title (svref local-time:+month-names+ month)
        :href (restas:genurl 'archive-for-month
                             :year year
                             :month (format nil "~2,'0D" month))))

(defun archive-for-day-link (year month day)
  (list :title day
        :href (restas:genurl 'archive-for-day
                             :year year
                             :month (format nil "~2,'0D" month)
                             :day (format nil "~2,'0D" day))))

(defun prepare-post-data (post)
  (let* ((published (gethash "published" post))
         (year (local-time:timestamp-year published))
         (month (local-time:timestamp-month published))
         (day (local-time:timestamp-day published)))
    (list :title (gethash "title" post)
          :href (restas:genurl 'one-post
                               :year year
                               :month (format nil "~2,'0D" month)
                               :day (format nil "~2,'0D" day)
                               :title (gethash "title" post))
          :content (gethash "content" post)
          :all-tags-href (restas:genurl 'all-tags)
          :tags (iter (for tag in (gethash "tags" post))
                      (collect
                          (list :name tag
                                :href (restas:genurl 'posts-with-tag :tag tag))))
          :published (list :year (archive-for-year-link year)
                           :month (archive-for-month-link year month)
                           :day (archive-for-day-link year month day)))))

(defmethod render-tagged-data ((type (eql :list-posts-page)) &key posts navigation)
  (arblog.view:show-all-blog-post
   (list :posts (mapcar 'prepare-post-data posts)
         :navigation navigation)))

(defmethod render-tagged-data ((type (eql :archive-for-year)) &key year months)
  (arblog.view:archive-for-year
   (list :year year
         :months (iter (for month in months)
                       (collect (archive-for-month-link year month))))))

(defmethod render-tagged-data ((type (eql :archive-for-month)) &key year month posts)
  (arblog.view:archive-for-month
   (list :posts (mapcar 'prepare-post-data posts)
         :year year
         :month (svref local-time:+month-names+ month))))

(defmethod render-tagged-data ((type (eql :archive-for-day)) &key year month day posts)
  (arblog.view:archive-for-day
   (list :posts (mapcar 'prepare-post-data posts)
         :year year
         :month (svref local-time:+month-names+ month)
         :day day)))  

(defmethod render-tagged-data ((type (eql :one-post-page)) &key post
                               &aux (id (gethash "_id" post)))
  (arblog.view:show-one-post (list* :disqus (list :shortname *disqus-shortname*
                                                  :developer-mode *disqus-developer-mode*
                                                  :identifier id
                                                  :permalink (restas:gen-full-url 'post-permalink :id id))
                                    (prepare-post-data post))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-tagged-data ((type (eql :tags-page)) &key tags)
  (arblog.view:tags-page
   (list :tags
         (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
               (collect (list :href (restas:genurl 'posts-with-tag
                                                   :tag tag)
                              :name tag))))))

(defmethod render-tagged-data ((type (eql :posts-with-tag-page)) &key tag posts navigation)
  (arblog.view:post-with-tag-page
   (list :tag tag
         :atom-feed-href (restas:genurl 'posts-with-tag-feed :tag tag)
         :navigation navigation
         :posts (mapcar 'prepare-post-data posts))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Atom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod render-tagged-data ((type (eql :atom-feed)) &key name href-atom href-html posts)
  (arblog.view:atom-feed
   (list :name name
         :href-atom href-atom
         :href-html href-html
         :posts (iter (for post in posts)
                      (collect
                          (list :id (gethash "_id" post)
                                :title (gethash "title" post)
                                :link (restas:genurl 'post-permalink :id (gethash "_id" post))
                                :published (local-time:format-timestring nil (gethash "published" post))
                                :updated (local-time:format-timestring nil (gethash "updated" post))
                                :content (gethash "content" post)))))))
