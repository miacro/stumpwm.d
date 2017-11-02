;;;; package.lisp

(defpackage #:desktop-entry
  (:use #:cl #:stumpwm)
  (:export :show-menu
           :load-desktop-file
           :load-menu-file
           :init-menu))

