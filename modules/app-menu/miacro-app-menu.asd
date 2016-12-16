;;;; miacro-app-menu.asd

(asdf:defsystem #:miacro-app-menu
  :description "miacro-app-menu"
  :author "fqguozhou@gmail.com"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "miacro-app-menu")))

