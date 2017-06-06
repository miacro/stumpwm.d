;;;; freetype-fonts.asd

(asdf:defsystem #:freetype-fonts
  :serial t
  :description "Describe freetype-fonts here"
  :author "miacro <fqguozhou@gmail.com>"
  :license "GPLv3"
  :depends-on (#:stumpwm #:cl-freetype2)
  :components ((:file "package")
               (:file "freetype-fonts")))

