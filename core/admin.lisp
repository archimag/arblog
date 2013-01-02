;;;; admin.lisp

(in-package #:arblog.admin)

(defparameter *post-permalink-route* nil)

(defun form-action-p (method)
  (alexandria:named-lambda required-form-action ()
    (hunchentoot:post-parameter method)))

(defun post-parameter-tags ()
  (iter (for tag in (split-sequence:split-sequence #\, (hunchentoot:post-parameter "tags")))
        (collect (string-trim #(#\Space #\Tab) tag))))

;; main admin page

(restas:define-route entry ("/")
  (:apply-render-method 'render.admin-posts)
  (:additional-variables (skip (parse-skip-param) 0))
  (let ((*posts-on-page* 25))
    (list (ds.list-recent-posts skip *posts-on-page*
                                :fields '("title" "published"))
          (navigation (restas:genurl 'entry)
                      skip
                      (ds.count-posts)))))

;; create post

(restas:define-route create-post ("create-post")
  (:apply-render-method #'render.admin-edit-post))

(restas:define-route cancel-create-post ("create-post" :method :post)
  (:requirement (form-action-p "cancel"))
  (restas:redirect 'entry))

(restas:define-route preview-create-post ("create-post" :method :post)
  (:requirement (form-action-p "preview"))
  (:apply-render-method #'render.admin-edit-post)
  (:additional-variables (markup (hunchentoot:post-parameter "content"))
                         (title (hunchentoot:post-parameter "title"))
                         (tags (post-parameter-tags)))
  (list :title title
        :markup markup
        :tags tags
        :preview (markup.render-content markup)))

(restas:define-route save-create-post ("create-post" :method :post)
  (:requirement (form-action-p "save"))
  (:additional-variables (markup (hunchentoot:post-parameter "content"))
                         (title (hunchentoot:post-parameter "title"))
                         (tags (post-parameter-tags)))
  (let* ((id (ds.insert-post title tags (markup.render-content markup) :markup markup)))
    (if *post-permalink-route*
        (restas:redirect *post-permalink-route* :id id)
        (restas:redirect 'entry))))

;; edit post

(restas:define-route edit-post (":id")
  (:apply-render-method 'render.admin-edit-post)
  (let* ((post (ds.get-single-post id)))
    (list :title (gethash "title" post)
          :markup (gethash "markup" post)
          :tags (gethash "tags" post)
          :preview (gethash "content" post))))

(restas:define-route cancel-edit-post (":id" :method :post)
  (:requirement (form-action-p "cancel"))
  (declare (ignore id))
  (restas:redirect 'entry))

(restas:define-route preview-edit-post (":id" :method :post)
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

(restas:define-route save-edit-post (":id" :method :post)
  (:requirement (form-action-p "save"))
  (:additional-variables (markup (hunchentoot:post-parameter "content"))
                         (title (hunchentoot:post-parameter "title"))
                         (tags (post-parameter-tags)))
  (ds.update-post id
                  title
                  tags
                  (markup.render-content markup)
                  :markup markup)
  (if *post-permalink-route*
      (restas:redirect *post-permalink-route* :id id)
      (restas:redirect 'entry)))
