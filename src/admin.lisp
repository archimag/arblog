;;;; route.lisp

(in-package #:arblog)

;; (restas:define-route admin-entry ("admin/")
;;   (let ((skip (or (ignore-errors (parse-integer (hunchentoot:get-parameter "skip"))) 0)))
;;     (list :admin-posts-page
;;           :navigation (navigation (restas:genurl 'admin-entry)
;;                                   skip
;;                                   (st.count-posts))
;;           :posts (st.list-recent-posts skip 30 :fields '("title" "published")))))

;; (restas:define-route admin-edit-post ("admin/:id")
;;   (list :admin-edit-post-page
;;         :post (st.find-single-post year month day title)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-view-tagged-data (type (&rest args) &body body)
  `(defmethod render-tagged-data ((type (eql ,type)) &key ,@args)
     ,@body))

(defun render-published (published)
  (format nil
          "~A.~A.~A"
         (local-time:timestamp-year published)
         (local-time:timestamp-month published)
         (local-time:timestamp-day published)))
  

(define-view-tagged-data :admin-posts-page (posts navigation)
  (arblog.view:admin-post-page
   (list :posts (iter (for post in posts)
                      (collect (list :id (gethash "_id" post)
                                     :title (gethash "title" post)
                                     :published (render-published (gethash "published" post)))))
         :navigation navigation)))

;;(defmethod render-tagged-data ((type (eql :admin-posts-page)) &key posts navigation)
  
