;;;; defmodule.lisp

(restas:define-module #:arblog
  (:use #:cl #:iter)
  (:export #:*disqus-enabled*
           #:*disqus-shortname*
           #:*disqus-developer-mode*

           #:*posts-on-page*
           #:*blog-name*

           #:register-theme-static-dir

           #:parse-skip-param
           #:navigation
           #:title-to-urlname

           #:@admin))

(restas:define-policy #:datastore
  (:interface-package #:arblog.policy.datastore)
  (:interface-method-template "DATASTORE-~A")
  (:internal-package #:arblog.internal.datastore)
  (:internal-function-template "DS.~A")
  
  (define-method count-posts (&optional tag)
    "Return a count of the posts that are published")
  
  (define-method list-recent-posts (skip limit &key tag fields)
    "Retrieve the recent posts.")
  
  (define-method find-single-post (year month day urlname)
    "Retrieve a single post, based on date and post urlname")

  (define-method get-single-post (id &key fields)
    "Retrieve a single post, based  on post ID")

  (define-method list-archive-posts (min max &optional fields)
    "Retrieve archive posts")

  (define-method all-tags ()
    "Retrieve an array of tags")

  (define-method insert-post (title tags content &key markup published updated)
    "Insert post in the datastore and return the post ID of the created post")

  (define-method update-post (id title tags content &key markup)
    "Update post in the datastore")

  (define-method set-admin (name password)
    "Set administrator name and password")

  (define-method check-admin (name password)
    "Check for administrator rights"))

(restas:define-policy #:markup
  (:interface-package #:arblog.policy.markup)
  (:interface-method-template "MARKUP-~A")
  (:internal-package #:arblog.internal.markup)
  (:internal-function-template "MARKUP.~A")

  (define-method render-content (content)
    "Generate HTML from markup"))

(restas:define-policy #:theme
  (:interface-package #:arblog.policy.theme)
  (:interface-method-template "THEME-~A")
  (:internal-package #:arblog.internal.theme)
  (:internal-function-template "RENDER.~A")

  (define-method list-recent-posts (posts navigation))
  
  (define-method archive-for-year (year months))
  (define-method archive-for-month (year month posts))
  (define-method archive-for-day (year month day posts))
  (define-method one-post (post))

  (define-method all-tags (tags))
  (define-method posts-with-tag (tag posts navigation))

  (define-method admin-posts (posts navigation))
  (define-method admin-edit-post (&key title markup tags preview)))


(restas:define-module #:arblog.public
  (:use #:cl #:iter #:arblog
        #:arblog.internal.datastore
        #:arblog.internal.theme
        #:arblog.internal.markup))

(restas:define-module #:arblog.admin
  (:use #:cl #:iter #:arblog
        #:arblog.internal.datastore
        #:arblog.internal.theme
        #:arblog.internal.markup)
  (:export #:*post-permalink-route*))

(restas:define-module #:arblog.static
  (:use #:cl #:iter #:arblog))
