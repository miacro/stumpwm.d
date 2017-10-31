(require :desktop-entry)
(in-package #:desktop-entry)
(add-to-entry-list (get-entry-from-desktop-file #P"/usr/share/applications/google-chrome.desktop"))
(add-to-entry-list #P"/usr/share/applications/google-chrome.desktop")
(format t "first entry: ~A~%" (categories (first *entry-list*)))
(format t "second entry: ~A~%" (categories (second *entry-list*)))
(init-entry-list)
(format t "entry count: ~A~%" (length *entry-list*))