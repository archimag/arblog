;;;; arblog.asd

(defsystem #:arblog
  :depends-on (#:restas #:mongo-cl-driver #:closure-template #:restas-directory-publisher #:ironclad #:docutils #:colorize)
  :pathname "src/"
  :serial t
  :components ((:file "defmodule") (:file "datastore") (:file "markup") (:file "render") (:file "routes")))
