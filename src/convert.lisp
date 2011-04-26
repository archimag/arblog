;;;; convert.lisp

(in-package #:arblog)

(defparameter *xpath-atom-entry*
  "/atom:feed/atom:entry[thr:total and not(app:control/app:draft)]")

(defparameter *xpath-entry-tags*
  "atom:category[@scheme='http://www.blogger.com/atom/ns#']/@term")

(defun calc-sha1-id (title published)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1 (babel:string-to-octets (format nil "~A~A" title published)
                                  :encoding :utf-8))))

(defun import-from-atom-feed (path blog)
  (xtree:with-parse-document (feed path)
    (let ((xpath:*default-ns-map* '(("atom" "http://www.w3.org/2005/Atom")
                                    ("thr" "http://purl.org/syndication/thread/1.0")
                                    ("app" "http://purl.org/atom/app#")))
          (posts (mongo:collection blog "posts")))

      (iter (for rawentry in-xpath-result *xpath-atom-entry*  on feed)
            (for entry = (son))
            (for title = (xpath:find-string rawentry "atom:title"))
            (for published = (local-time:parse-timestring (xpath:find-string rawentry "atom:published")))

            (setf (gethash "_id" entry)
                  (calc-sha1-id title published))

            (setf (gethash "title" entry)
                  title)
            
            (setf (gethash "published" entry)
                  published)

            (setf (gethash "updated" entry)
                  (local-time:parse-timestring (xpath:find-string rawentry "atom:updated")))

            (setf (gethash "content" entry)
                  (xpath:find-string rawentry "atom:content"))

            (setf (gethash "tags" entry)
                  (map 'vector 
                       #'xtree:text-content
                       (xpath:find-list rawentry *xpath-entry-tags*)))

            (mongo:insert-op posts entry)))))