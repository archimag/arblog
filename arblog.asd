;;;; arblog.asd

(defsystem #:arblog
  :depends-on (#:restas #:mongo-cl-driver #:closure-template)
  :components
  ((:module "src"
            :components
            ((:file "defmodule")
             (:file "render" :depends-on ("defmodule"))
             (:file "routes" :depends-on ("render"))))))