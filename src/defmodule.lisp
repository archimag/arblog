;;;; defmodule.lisp

(restas:define-module #:arblog
  (:use #:cl #:iter #:son-sugar))

(in-package #:arblog)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *basepath*
    (make-pathname :directory (pathname-directory (asdf:component-pathname (asdf:find-system '#:arblog))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (closure-template:compile-template :common-lisp-backend
   (fad:list-directory (merge-pathnames "templates/" *basepath*))))

(defun prepare-post (post &aux (entry (alexandria:copy-hash-table post)))
  (let ((doc (docutils:read-document (gethash :content entry)
                                     (make-instance 'docutils.parser.rst:rst-reader)))
        (writer (make-instance 'docutils.writer.html:html-writer)))
    (docutils:visit-node writer doc)
    (setf (gethash :content entry)
          (with-output-to-string (out)
            (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                                  docutils.writer.html:docinfo
                                  docutils.writer.html:body))
                  (docutils:write-part writer part out))
            (format out "</div>")))
    (alexandria:hash-table-plist entry)))

(restas:define-route entry ("" :render-method 'arblog.view:show-all-blog-post)
  (let ((mongo.bson:*bson-identifier-name-to-lisp* #'camel-case:camel-case-to-lisp))
    (mongo:with-database (blog "blog")
      (list :posts
            (mapcar 'prepare-post
                    (mongo:find-list (mongo:collection blog "posts")
                                     (son "$query" (son)
                                          "$orderby" (son "published" 1))))))))
  
(defun parse-atom-feed (path)
  (xtree:with-parse-document (feed path)
    (mongo:with-database (blog "blog")
      (let ((xpath:*default-ns-map* '(("atom" "http://www.w3.org/2005/Atom")))
            (posts (mongo:collection blog "posts")))

      (iter (for rawentry in-xpath-result "/atom:feed/atom:entry" on feed)
            (let ((entry (son)))
              (setf (gethash "title" entry)
                    (xpath:find-string rawentry "atom:title"))
              
              (setf (gethash "published" entry)
                    (local-time:parse-timestring (xpath:find-string rawentry "atom:published")))

              (setf (gethash "updated" entry)
                    (local-time:parse-timestring (xpath:find-string rawentry "atom:updated")))

              (setf (gethash "content" entry)
                    (with-input-from-string (in (xpath:find-string rawentry "atom:content"))
                      (with-output-to-string (s)
                        (external-program:run "/usr/bin/pandoc"
                                              (list "-f" "html" "-t" "rst" "-")
                                              :input in
                                              :output s))))
              (mongo:insert-op posts
                               entry))))))) 


                      
