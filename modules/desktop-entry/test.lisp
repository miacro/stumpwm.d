(require :desktop-entry)
(in-package #:desktop-entry)
(format t "====================~%")
(add-to-entry-list (get-entry-from-desktop-file #P"/usr/share/applications/google-chrome.desktop"))
(add-to-entry-list #P"/usr/share/applications/google-chrome.desktop")
(add-to-entry-list #P"/usr/share/applications/okularApplication_comicbook.desktop")
(format t "entry count: ~S~%" (length *entry-list*))
(format t "first entry: ~S~%" (categories (first *entry-list*)))
(format t "second entry: ~S~%" (categories (second *entry-list*)))
(format t "third entry: ~S~%" (no-display (third *entry-list*)))
(init-entry-list)
(format t "entry count: ~S~%" (length *entry-list*))

(defvar *menu*)
(setf *menu* (get-menu-by-categories "Network;System"))
(format t "menu: ~S~%" *menu*)
(setf *menu* (get-menu-by-categories "Qt"))
(format t "menu: ~S~%" *menu*)

(setf *menu* (build-menu))
(format t "menu: ~S~%" *menu*)

(add-category (first *entry-list*) *favorite-category*)
(format t "favorite categories: ~A~%" (categories (first *entry-list*)))
(add-category (first *entry-list*) *favorite-category*)
(format t "favorite categories: ~A~%" (categories (first *entry-list*)))
(add-category (third *entry-list*) *favorite-category*)
(set-entry-favorite "Google Chrome")
(setf *menu* (build-menu *favorite-category*))
(format t "menu: ~S~%" *menu*)

(format t "all categories: ~S~%" (get-all-categories))
(format t "entry list /System/Utility/:~%")
(dolist (entry (filter-entry-by-categories '("System" "Utility")))
  (format t "~A~%" entry))

(format t "entry list /System/:~%")
(dolist (entry (filter-entry-by-categories '("System" "Utility")))
  (format t "~A~%" entry))

(format t "grouped entrys: ~S~%" (group-entry-by-categories 
                                    :entry-list (filter-entry-by-categories '("System"))))

(format t "menu ~S~%" (build-menu2))

