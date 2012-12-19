;;;; arblog.asd

(defsystem #:arblog
  :depends-on (#:restas #:closure-template #:restas-directory-publisher )
  :pathname "core/"
  :serial t
  :components ((:file "defmodule") (:file "view") (:file "routes")))
