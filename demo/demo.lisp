;;;; demo.lisp

(asdf:operate 'asdf:load-op '#:arblog)
(asdf:operate 'asdf:load-op '#:arblog-systems)

(asdf:operate 'asdf:load-op '#:arblog-datastore-mongodb)

(asdf:operate 'asdf:load-op '#:arblog-markup-rst)
;; (asdf:operate 'asdf:load-op '#:arblog-markup-markdown)

(asdf:operate 'asdf:load-op '#:arblog-theme-mirev)

(restas:define-module #:myblog
  (:use #:cl))

(in-package #:myblog)

(restas:mount-submodule -arblog- (#:arblog)
  (arblog:*blog-name* "My blog")
  (arblog:*posts-on-page* 10)
  
  (arblog:*datastore* (make-instance 'arblog.datastore.mongodb:arblog-mongo-datastore))
  (arblog:*markup* (make-instance 'arblog.markup.rst:arblog-rst-markup))
  ;;(arblog:*markup* (make-instance 'arblog.markup.markdown:arblog-markdown-markup))
  (arblog:*theme* (make-instance 'arblog.theme.mirev:arblog-mirev-theme))
  
  (arblog:*disqus-enabled* nil))
  
(restas:start '#:myblog :port 8080)
