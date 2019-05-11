;;;; scrot.asd

(asdf:defsystem #:scrot
  :description "scrot"
  :author "fqguozhou@gmail.com"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "scrot")))
