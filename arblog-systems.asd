;;;; arblog-policies.asd

(defsystem #:arblog-systems)

;;;; datastores

(defsystem #:arblog-datastore-mongodb
  :depends-on (#:arblog #:mongo-cl-driver #:ironclad)
  :pathname "policies/datastore/"
  :components ((:file "mongodb")))

;;;; markups

(defsystem #:arblog-markup-rst
  :depends-on (#:arblog #:docutils #:colorize)
  :pathname "policies/markup/"
  :components ((:file "rst")))

;;;; themes

(defsystem #:arblog-theme-mirev
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:arblog)
  :pathname "policies/theme/mirev/"
  :serial t
  :components ((:module "templates"
                        :components ((:closure-template "page")
                                     (:closure-template "entries")
                                     (:closure-template "tags")
                                     (:closure-template "archive")
                                     (:closure-template "feed")
                                     (:closure-template "admin")))
               (:file "mirev")))
