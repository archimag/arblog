;;;; mirev.lisp

(defpackage #:arblog.theme.mirev
  (:use #:cl #:iter #:arblog.policy.theme)
  (:export #:arblog-mirev-theme
           #:theme-templates-package))

(in-package #:arblog.theme.mirev)

(defclass arblog-mirev-theme ()
  ((templates-package :initarg :templates-package
                      :initform '#:arblog.theme.mirev.tmpl
                      :reader theme-templates-package)))

(arblog:register-theme-static-dir
 "mirev"
 (merge-pathnames "static/" (asdf:component-pathname  (asdf:find-system '#:arblog-theme-mirev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme-render-tagged-data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-page-view (type (&rest args) &body body)
  (alexandria:with-unique-names (tmplname tmplargs theme)
    `(defmethod theme-render-tagged-data ((,theme arblog-mirev-theme) (type (eql ,type)) &key ,@args)
       (macrolet ((render-template (,tmplname &body ,tmplargs)
                    `(closure-template:ttable-call-template
                      (closure-template:package-ttable (theme-templates-package ,',theme))
                      (string ',,tmplname)
                      (list* :blog-name arblog:*blog-name*
                             ,@,tmplargs))))
         ,@body))))

(defun archive-for-year-link (year)
  (list :title year
        :href (restas:genurl 'arblog:archive-for-year
                             :year year)))

(defun archive-for-month-link (year month)
  (list :title (svref local-time:+month-names+ month)
        :href (restas:genurl 'arblog:archive-for-month
                             :year year
                             :month (format nil "~2,'0D" month))))

(defun archive-for-day-link (year month day)
  (list :title day
        :href (restas:genurl 'arblog:archive-for-day
                             :year year
                             :month (format nil "~2,'0D" month)
                             :day (format nil "~2,'0D" day))))

(defun prepare-post-data (post)
  (let* ((published (gethash "published" post))
         (year (local-time:timestamp-year published))
         (month (local-time:timestamp-month published))
         (day (local-time:timestamp-day published)))
    (list :id (gethash "_id" post)
          :title (gethash "title" post)
          :href (restas:genurl 'arblog:one-post
                               :year year
                               :month (format nil "~2,'0D" month)
                               :day (format nil "~2,'0D" day)
                               :title (gethash "title" post))
          :content (gethash "content" post)
          :content-rst (gethash "content-rst" post)
          :all-tags-href (restas:genurl 'arblog:all-tags)
          :tags (iter (for tag in (gethash "tags" post))
                      (collect
                          (list :name tag
                                :href (restas:genurl 'arblog:posts-with-tag :tag tag))))
          :published (list :year (archive-for-year-link year)
                           :month (archive-for-month-link year month)
                           :day (archive-for-day-link year month day)))))

(define-page-view :list-posts-page (posts navigation)
  (render-template show-all-blog-post
    (list :posts (mapcar 'prepare-post-data posts)
          :disqus (list :enabled arblog:*disqus-enabled*
                        :shortname arblog:*disqus-shortname*)
          :navigation navigation)))

(define-page-view :archive-for-year (year months)
  (render-template archive-for-year
    (list :year year
          :months (iter (for month in months)
                        (collect (archive-for-month-link year month))))))

(define-page-view :archive-for-month (year month posts)
  (render-template archive-for-month
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (svref local-time:+month-names+ month))))

(define-page-view :archive-for-day (year month day posts)
  (render-template archive-for-day
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (svref local-time:+month-names+ month)
          :day day)))

(define-page-view :one-post-page (post)
  (let ((id (gethash "_id" post)))
    (render-template show-one-post
      (list* :disqus (list :shortname arblog:*disqus-shortname*
                           :developer-mode arblog:*disqus-developer-mode*
                           :enabled arblog:*disqus-enabled*
                           :identifier id
                           :permalink (restas:gen-full-url 'post-permalink :id id))
             (prepare-post-data post)))))


;;;; Tags

(define-page-view :tags-page (tags)
  (render-template tags-page
    (list :tags
          (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
                (collect (list :href (restas:genurl 'arblog:posts-with-tag
                                                    :tag tag)
                               :name tag))))))

(define-page-view :posts-with-tag-page (tag posts navigation)
  (render-template post-with-tag-page
    (list :tag tag
          :atom-feed-href (restas:genurl 'arblog:posts-with-tag-feed :tag tag)
          :navigation navigation
          :posts (mapcar 'prepare-post-data posts))))

;;;; Atom

(define-page-view :atom-feed (name href-atom href-html posts)
  (render-template atom-feed
    (list :name name
          :href-atom href-atom
          :href-html href-html
          :posts (iter (for post in posts)
                       (collect
                           (list :id (gethash "_id" post)
                                 :title (gethash "title" post)
                                 :link (restas:gen-full-url 'arblog:post-permalink :id (gethash "_id" post))
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

(define-page-view :admin-posts-page (posts navigation)
  (render-template admin-post-page
    (list :posts (iter (for post in posts)
                       (collect (list :id (gethash "_id" post)
                                      :title (gethash "title" post)
                                      :href (restas:genurl 'arblog:admin-edit-post :id (gethash "_id" post))
                                      :published (render-published (gethash "published" post)))))
          :navigation navigation
          :create-post-href (restas:genurl 'arblog:admin-create-post))))

(define-page-view :admin-edit-post-page (post)
  (render-template admin-edit-post-page
    (list :post (prepare-post-data post))))

(define-page-view :admin-create-post-page ()
  (render-template admin-edit-post-page nil))

(define-page-view :admin-preview-post-page (title content-rst tags preview)
  (render-template admin-edit-post-page
    (list :post (list :title title
                      :content-rst content-rst
                      :tags (iter (for tag in tags)
                                  (collect (list :name tag))))
          :preview preview)))
