;;;; routes.lisp

(in-package #:arblog)

(defun parse-skip-param ()
  (ignore-errors (parse-integer (hunchentoot:get-parameter "skip"))))

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

(defmethod data-sift:compile-rule ((rule (eql 'year)) &key)
  (data-sift:compile-rule 'integer))

(defmethod data-sift:compile-rule ((rule (eql 'month)) &key)
  (data-sift:compile-rule '(integer :min-value 1 :max-value 12)))

(defmethod data-sift:compile-rule ((rule (eql 'day)) &key)
  (data-sift:compile-rule '(integer :min-value 1 :max-value 31)))

;;;; main page

(restas:define-route entry ("")
  (:apply-render-method #'render.list-recent-posts)
  (:additional-variables (skip (parse-skip-param) 0))
  (list (ds.list-recent-posts skip *posts-on-page*)
        (navigation (restas:genurl 'entry)
                    skip
                    (ds.count-posts))))

;;;; One post

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
                     :month (local-time:timestamp-month published)
                     :day (local-time:timestamp-day published)
                     :urlname urlname)))

;;;; archive

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
        :link (restas:gen-full-url 'post-permalink :id (gethash "_id" post))
        :published (local-time:format-timestring nil (gethash "published" post))
        :updated (local-time:format-timestring nil (gethash "updated" post))
        :content (gethash "content" post)))  

(restas:define-route posts-feed ("feeds/atom" :content-type "application/atom+xml")
  (:render-method #'arblog.feed.tmpl:atom-feed)
  (list :name *blog-name*
        :href-atom (restas:gen-full-url 'posts-feed)
        :href-html (restas:gen-full-url 'entry)
        :posts (mapcar #'feed-post-info (ds.list-recent-posts 0 50))))

(restas:define-route posts-with-tag-feed ("feeds/atom/tag/:tag" :content-type "application/atom+xml")
  (:render-method #'arblog.feed.tmpl:atom-feed)
  (list :name (format nil "~A blog posts with tag \"~A\"" *blog-name* tag)
        :href-atom (restas:gen-full-url 'posts-feed)
        :href-html (restas:gen-full-url 'entry)
        :posts (mapcar #'feed-post-info (ds.list-recent-posts 0 50))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Admin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass admin-route (routes:proxy-route) ())

(defmethod restas:process-route :before ((route admin-route) bindings)
  (multiple-value-bind (user password) (hunchentoot:authorization)
    (unless (ds.check-admin user password)
      (hunchentoot:require-authorization))))

(defun @admin (route)
  (make-instance 'admin-route :target route))

(defun form-action-p (method)
  (alexandria:named-lambda required-form-action ()
    (hunchentoot:post-parameter method)))

(defun post-parameter-tags ()
  (iter (for tag in (split-sequence:split-sequence #\, (hunchentoot:post-parameter "tags")))
        (collect (string-trim #(#\Space #\Tab) tag))))

(defun preview-post (&aux (content-markup (hunchentoot:post-parameter "content")))
  (render.admin-edit-post :title (hunchentoot:post-parameter "title")
                          :markup content-markup
                          :tags (post-parameter-tags)
                          :preview (markup.render-content content-markup)))

;; main admin page

(restas:define-route admin-entry ("admin/")
  (:decorators #'@admin)
  (:apply-render-method 'render.admin-posts)
  (:additional-variables (skip (parse-skip-param) 0))
  (let ((*posts-on-page* 25))
    (list (ds.list-recent-posts skip *posts-on-page*
                                :fields '("title" "published"))
          (navigation (restas:genurl 'admin-entry)
                      skip
                      (ds.count-posts)))))

;; create post

(restas:define-route admin-create-post ("admin/create-post")
  (:decorators '@admin)
  (:apply-render-method #'render.admin-edit-post))

(restas:define-route admin-cancel-create-post ("admin/create-post" :method :post)
  (:requirement (form-action-p "cancel"))
  (:decorators '@admin)
  (restas:redirect 'admin-entry))

(restas:define-route admin-preview-create-post ("admin/create-post" :method :post)
  (:requirement (form-action-p "preview"))
  (:decorators '@admin)
  (:apply-render-method #'render.admin-edit-post)
  (:additional-variables (markup (hunchentoot:post-parameter "content"))
                         (title (hunchentoot:post-parameter "title"))
                         (tags (post-parameter-tags)))
  (list :title title
        :markup markup
        :tags tags
        :preview (markup.render-content markup)))

(restas:define-route admin-save-create-post ("admin/create-post" :method :post)
  (:requirement (form-action-p "save"))
  (:decorators '@admin)
  (:additional-variables (markup (hunchentoot:post-parameter "content"))
                         (title (hunchentoot:post-parameter "title"))
                         (tags (post-parameter-tags)))
  (let* ((id (ds.insert-post title tags (markup.render-content markup) :markup markup)))
    (restas:redirect 'post-permalink :id id)))

;; edit post

(restas:define-route admin-edit-post ("admin/:id")
  (:decorators '@admin)
  (:apply-render-method 'render.admin-edit-post)
  (let* ((post (ds.get-single-post id)))
    (list :title (gethash "title" post)
          :markup (gethash "markup" post)
          :tags (gethash "tags" post)
          :preview (gethash "content" post))))

(restas:define-route admin-cancel-edit-post ("admin/:id" :method :post)
  (:decorators '@admin)
  (:requirement (form-action-p "cancel"))
  (declare (ignore id))
  (restas:redirect 'admin-entry))

(restas:define-route admin-preview-edit-post ("admin/:id" :method :post)
  (:decorators '@admin)
  (:requirement (form-action-p "preview"))
  (:apply-render-method #'render.admin-edit-post)
  (:additional-variables (markup (hunchentoot:post-parameter "content"))
                         (title (hunchentoot:post-parameter "title"))
                         (tags (post-parameter-tags)))
  (declare (ignore id))
  (list :title title
        :markup markup
        :tags tags
        :preview (markup.render-content markup)))

(restas:define-route admin-save-edit-post ("admin/:id" :method :post)
  (:decorators '@admin)
  (:requirement (form-action-p "save"))
  (:additional-variables (markup (hunchentoot:post-parameter "content"))
                         (title (hunchentoot:post-parameter "title"))
                         (tags (post-parameter-tags)))
  (ds.update-post id
                  title
                  tags
                  (markup.render-content markup)
                  :markup markup)
  (restas:redirect 'post-permalink :id id))

;;;; static files

(defun parse-native-namestring (thing)
  #+sbcl (sb-ext:parse-native-namestring thing)
  #-sbcl (parse-namestring thing))

(restas:define-route theme-static-file ("/static/:theme/*path")
  (let* ((theme-path (gethash theme *theme-static-dir-map*))
         (relative-path (parse-native-namestring (format nil "~{~A~^/~}" path)))
         (file (merge-pathnames relative-path theme-path)))
    (when (find :up (pathname-directory relative-path))
      (restas:abort-route-handler hunchentoot:+http-bad-request+))
    (unless (fad:file-exists-p file)
      (restas:abort-route-handler hunchentoot:+http-not-found+))
    file))
