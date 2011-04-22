;;;; arblog.asd

(defsystem #:arblog
  :depends-on (#:restas #:mongo-cl-driver #:closure-template #:sanitize #:external-programm #:docutils)
  :components
  ((:module "src"
            :components
            ((:file "defmodule")))))