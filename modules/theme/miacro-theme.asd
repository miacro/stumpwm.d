;;;; miacro-theme.asd

(asdf:defsystem #:miacro-theme
  :description "miacro-theme"
  :author "fqguozhou@gmail.com"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "miacro-theme")))

