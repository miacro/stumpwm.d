;;;; package.lisp

(defpackage #:freetype-fonts
  (:use #:cl #:stumpwm))

(in-package #:freetype-fonts)

(import '(stumpwm::font-exists-p stumpwm::open-font stumpwm::close-font
  stumpwm::font-ascent stumpwm::font-descent stumpwm::text-line-width
  stumpwm::draw-image-glyphs stumpwm::font-height))
