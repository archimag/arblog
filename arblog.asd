;;;; arblog.asd

(defsystem #:arblog
  :depends-on (#:restas #:mongo-cl-driver #:closure-template #:restas-directory-publisher #:ironclad #:docutils #:colorize)
  :components
  ((:module "src"
            :components
            ((:file "defmodule")
             (:file "render" :depends-on ("defmodule"))
             (:file "routes" :depends-on ("render"))
             (:file "markup" :depends-on ("defmodule"))
             (:file "utils" :depends-on ("routes"))))))