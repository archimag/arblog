;;;; render.lisp

(in-package #:arblog)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass view () ())

(defgeneric render-tagged-data (pagetype &rest args &key &allow-other-keys))

(defmethod restas:render-object ((view view) (data list))
  (apply 'render-tagged-data (car data) (cdr data)))

(defmacro define-tagged-data-view (type (&rest args) &body body)
  `(defmethod render-tagged-data ((type (eql ,type)) &key ,@args)
     ,@body))


(defun prepare-post-data (post)
  (let* ((published (gethash "published" post))
         (year (local-time:timestamp-year published))
         (month (local-time:timestamp-month published))
         (day (local-time:timestamp-day published)))
    (list :id (gethash "_id" post)
          :title (gethash "title" post)
          :href (restas:genurl 'one-post
                               :year year
                               :month (format nil "~2,'0D" month)
                               :day (format nil "~2,'0D" day)
                               :title (gethash "title" post))
          :content (gethash "content" post)
          :content-rst (gethash "content-rst" post)
          :all-tags-href (restas:genurl 'all-tags)
          :tags (iter (for tag in (gethash "tags" post))
                      (collect
                          (list :name tag
                                :href (restas:genurl 'posts-with-tag :tag tag))))
          :published (list :year (archive-for-year-link year)
                           :month (archive-for-month-link year month)
                           :day (archive-for-day-link year month day)))))

(define-tagged-data-view :list-posts-page (posts navigation)
  (arblog.view:show-all-blog-post
   (list :posts (mapcar 'prepare-post-data posts)
         :disqus (list :enabled *disqus-enabled*
                       :shortname *disqus-shortname*)
         :navigation navigation)))

(define-tagged-data-view :archive-for-year (year months)
  (arblog.view:archive-for-year
   (list :year year
         :months (iter (for month in months)
                       (collect (archive-for-month-link year month))))))

(define-tagged-data-view :archive-for-month (year month posts)
  (arblog.view:archive-for-month
   (list :posts (mapcar 'prepare-post-data posts)
         :year year
         :month (svref local-time:+month-names+ month))))

(define-tagged-data-view :archive-for-day (year month day posts)
  (arblog.view:archive-for-day
   (list :posts (mapcar 'prepare-post-data posts)
         :year year
         :month (svref local-time:+month-names+ month)
         :day day)))  

(define-tagged-data-view :one-post-page (post)
  (let ((id (gethash "_id" post)))
    (arblog.view:show-one-post (list* :disqus (list :shortname *disqus-shortname*
                                                    :developer-mode *disqus-developer-mode*
                                                    :enabled *disqus-enabled*
                                                    :identifier id
                                                    :permalink (restas:gen-full-url 'post-permalink :id id))
                                      (prepare-post-data post)))))

;;;; Tags

(define-tagged-data-view :tags-page (tags)
  (arblog.view:tags-page
   (list :tags
         (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
               (collect (list :href (restas:genurl 'posts-with-tag
                                                   :tag tag)
                              :name tag))))))

(define-tagged-data-view :posts-with-tag-page (tag posts navigation)
  (arblog.view:post-with-tag-page
   (list :tag tag
         :atom-feed-href (restas:genurl 'posts-with-tag-feed :tag tag)
         :navigation navigation
         :posts (mapcar 'prepare-post-data posts))))
  
;;;; Atom

(define-tagged-data-view :atom-feed (name href-atom href-html posts)
  (arblog.view:atom-feed
   (list :name name
         :href-atom href-atom
         :href-html href-html
         :posts (iter (for post in posts)
                      (collect
                          (list :id (gethash "_id" post)
                                :title (gethash "title" post)
                                :link (restas:gen-full-url 'post-permalink :id (gethash "_id" post))
                                :published (local-time:format-timestring nil (gethash "published" post))
                                :updated (local-time:format-timestring nil (gethash "updated" post))
                                :content (gethash "content" post)))))))

;;;; Admin

(defun render-published (published)
  (format nil
          "~A.~A.~A"
         (local-time:timestamp-year published)
         (local-time:timestamp-month published)
         (local-time:timestamp-day published)))
  

(define-tagged-data-view :admin-posts-page (posts navigation)
  (arblog.view:admin-post-page
   (list :posts (iter (for post in posts)
                      (collect (list :id (gethash "_id" post)
                                     :title (gethash "title" post)
                                     :href (restas:genurl 'admin-edit-post :id (gethash "_id" post))
                                     :published (render-published (gethash "published" post)))))
         :navigation navigation
         :create-post-href (restas:genurl 'admin-create-post))))


(define-tagged-data-view :admin-edit-post-page (post)
  (arblog.view:admin-edit-post-page
   (list :post (prepare-post-data post))))

(define-tagged-data-view :admin-create-post-page ()
  (arblog.view:admin-edit-post-page))

(define-tagged-data-view :admin-preview-post-page (title content-rst tags)
  (arblog.view:admin-edit-post-page
   (list :post (list :title title
                     :content-rst content-rst
                     :tags (iter (for tag in tags)
                                 (collect (list :name tag))))
         :preview (render-arblog-markup content-rst))))
