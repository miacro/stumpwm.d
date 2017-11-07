(require :desktop-entry)
(in-package #:desktop-entry)
(block test-desktop-entry
  (format t "====================~%")
  (format t "load-desktop-file: ~S~%" (load-desktop-file "/usr/share/applications/google-chrome.desktop"))
  (format t "load-desktop-file: ~S~%" (load-desktop-file "/usr/share/applications/emacsclient.desktop"))

  (setf *entry-list* 
    (add-to-entry-list *entry-list*
      (make-desktop-entry #P"/usr/share/applications/google-chrome.desktop")))
  (format t "entry count: ~S~%" (length *entry-list*))
  (format t "first entry: ~S~%" (categories (first *entry-list*)))

  (setf *entry-list* 
    (add-to-entry-list *entry-list* #P"/usr/share/applications/google-chrome.desktop"))
  (format t "entry count: ~S~%" (length *entry-list*))

  (setf *entry-list* 
    (add-to-entry-list *entry-list* #P"/usr/share/applications/okularApplication_comicbook.desktop"))
  (format t "second entry: ~S~%" (no-display (second *entry-list*)))

  (setf *entry-list* 
    (add-to-entry-list *entry-list* #P"/usr/share/applications/emacsclient.desktop"))
  (format t "third entry: ~S~%" (no-display (third *entry-list*)))
  (init-entry-list)
  (format t "entry count: ~S~%" (length *entry-list*))
  
  (add-category (first *entry-list*) "test")
  (format t "test categories: ~A~%" (categories (first *entry-list*)))
  (add-category (first *entry-list*) "test")
  (format t "test categories: ~A~%" (categories (first *entry-list*)))
  (add-category (third *entry-list*) "test")

  (add-favorite-entry "Google Chrome")
  (format t "favroite entry length: ~S~%" (length *favorite-list*))
  
  (format t "all categories: ~S~%" (get-all-categories *entry-list*))
  (format t "entry list /:~%")
  (dolist (entry (filter-entry-list *entry-list* :categories '()))
    (format t "~A~%" entry))
  (format t "entry list /System/Utility/:~%")
  (dolist (entry (filter-entry-list *entry-list* :categories '("System" "Utility")))
    (format t "~A~%" entry))
  
  (format t "entry list /System/:~%")
  (dolist (entry (filter-entry-list *entry-list* :categories '("System")))
    (format t "~A~%" entry))

  (format t "grouped entrys /System: ~S~%" 
    (group-by-categories (filter-entry-list *entry-list* :categories '("System"))))

  (format t "grouped entrys /: ~S~%" 
    (group-by-categories (filter-entry-list *entry-list* :categories '("AudioVideo"))))
  (format t "menu ~S~%" (build-menu '("AudioVideo") :min-entry-in-category 3))

  (format t "menu ~S~%" (build-menu '("Network") :min-entry-in-category 5))
  (return-from test-desktop-entry) 
)
