;;;; convert.lisp

(in-package #:arblog)

(defparameter *xpath-atom-entry*
  "/atom:feed/atom:entry[thr:total and not(app:control/app:draft)]")

(defparameter *xpath-entry-tags*
  "atom:category[@scheme='http://www.blogger.com/atom/ns#']/@term")

(defun parse-atom-feed (path)
  (xtree:with-parse-document (feed path)
    (mongo:with-database (blog "blog")
      (let ((xpath:*default-ns-map* '(("atom" "http://www.w3.org/2005/Atom")
                                      ("thr" "http://purl.org/syndication/thread/1.0")
                                      ("app" "http://purl.org/atom/app#")))
            (posts (mongo:collection blog "posts")))

        (iter (for rawentry in-xpath-result *xpath-atom-entry*  on feed)
              (for entry = (son))

              (setf (gethash "title" entry)
                    (xpath:find-string rawentry "atom:title"))
              
              (setf (gethash "published" entry)
                    (local-time:parse-timestring (xpath:find-string rawentry "atom:published")))

              (setf (gethash "updated" entry)
                    (local-time:parse-timestring (xpath:find-string rawentry "atom:updated")))

              (setf (gethash "content" entry)
                    (xpath:find-string rawentry "atom:content"))

              (setf (gethash "tags" entry)
                    (map 'vector 
                         #'xtree:text-content
                         (xpath:find-list rawentry *xpath-entry-tags*)))

              (mongo:insert-op posts entry))))))