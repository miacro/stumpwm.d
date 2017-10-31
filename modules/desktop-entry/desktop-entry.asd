;;;; desktop-entry.asd

(asdf:defsystem #:desktop-entry
  :description "desktop-entry"
  :author "fqguozhou@gmail.com"
  :license "GPLv3"
  :depends-on (#:stumpwm #:py-configparser)
  :serial t
  :components ((:file "package")
               (:file "desktop-entry")))

