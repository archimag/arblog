;;;; arblog.asd

(defsystem #:arblog
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas #:local-time)
  :pathname "core/"
  :serial t
  :components ((:file "defmodule") (:closure-template "feed") (:file "routes")))
