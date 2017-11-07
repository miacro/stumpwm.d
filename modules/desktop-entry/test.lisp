(require :desktop-entry)
(in-package #:desktop-entry)
(block test-desktop-entry
  (format t "====================~%")
  (format t "load-desktop-file: ~S~%" (load-desktop-file "/usr/share/applications/google-chrome.desktop"))
  (format t "load-desktop-file: ~S~%" (load-desktop-file "/usr/share/applications/emacsclient.desktop"))
  (add-to-entry-list (make-desktop-entry #P"/usr/share/applications/google-chrome.desktop"))
  (add-to-entry-list #P"/usr/share/applications/google-chrome.desktop")
  (add-to-entry-list #P"/usr/share/applications/okularApplication_comicbook.desktop")
  (add-to-entry-list #P"/usr/share/applications/emacsclient.desktop")
  (format t "entry count: ~S~%" (length *entry-list*))
  (format t "first entry: ~S~%" (categories (first *entry-list*)))
  (format t "second entry: ~S~%" (categories (second *entry-list*)))
  (format t "third entry: ~S~%" (no-display (third *entry-list*)))
  (format t "fourth entry: ~S~%" (no-display (fourth *entry-list*)))
  (init-entry-list)
  (format t "entry count: ~S~%" (length *entry-list*))
  
  (add-category (first *entry-list*) *favorite-category*)
  (format t "favorite categories: ~A~%" (categories (first *entry-list*)))
  (add-category (first *entry-list*) *favorite-category*)
  (format t "favorite categories: ~A~%" (categories (first *entry-list*)))
  (add-category (third *entry-list*) *favorite-category*)
  (set-entry-favorite "Google Chrome")
  
  (format t "all categories: ~S~%" (get-all-categories))
  (format t "entry list /:~%")
  (dolist (entry (filter-entry-by-categories '()))
    (format t "~A~%" entry))
  (format t "entry list /System/Utility/:~%")
  (dolist (entry (filter-entry-by-categories '("System" "Utility")))
    (format t "~A~%" entry))
  
  (format t "entry list /System/:~%")
  (dolist (entry (filter-entry-by-categories '("System")))
    (format t "~A~%" entry))

  (format t "grouped entrys /System: ~S~%" 
    (group-entry-by-categories :entry-list (filter-entry-by-categories '("System"))))

  (format t "grouped entrys /: ~S~%" 
    (group-entry-by-categories :entry-list (filter-entry-by-categories '("AudioVideo"))))
  (format t "menu ~S~%" (build-menu '("AudioVideo") :min-entry-in-category 3))

  (format t "menu ~S~%" (build-menu '("Network") :min-entry-in-category 5))
  (return-from test-desktop-entry) 
)
