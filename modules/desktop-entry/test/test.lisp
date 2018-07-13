(require :desktop-entry)
(require :fiveam)
(in-package #:desktop-entry)
(block test-desktop-entry
  (fiveam:def-suite test-desktop-entry-suite)
  (fiveam:in-suite test-desktop-entry-suite)
  (defconstant true-values
    '((:name "Google Chrome"
       :entry-type "Application"
       :exec "/usr/bin/google-chrome-stable %U"
       :path nil
       :categories ("Network" "WebBrowser")
       :no-display nil
       :only-show-in nil
       :terminal nil)
      (:name "GNU Emacs"
       :entry-type "Application"
       :exec "/usr/bin/emacs %F"
       :path nil
       :categories ("Development" "TextEditor")
       :no-display nil
       :only-show-in nil
       :terminal nil)))
  (fiveam:test init-desktop-entry
    (fiveam:is (equalp (load-desktop-file "google-chrome.desktop")
                       (first true-values)))
    (fiveam:is (equalp (load-desktop-file "emacs.desktop")
                       (second true-values))))
  (fiveam:test test-add-to-entry-list
    (let ((entry-list nil))
      (fiveam:is (= 1
                    (length
                     (add-to-entry-list
                      entry-list
                      (make-desktop-entry #P"google-chrome.desktop")))))))
  (fiveam:run! 'test-desktop-entry-suite)
  (return-from test-desktop-entry)
  (setf *entry-list*
        (add-to-entry-list *entry-list*
                           (make-desktop-entry #P"/usr/share/applications/google-chrome.desktop")))
  (format t "entry count: ~S~%" (length *entry-list*))
  (format t "first entry: ~S~%" (categories (first *entry-list*)))

  (setf *entry-list*
        (add-to-entry-list *entry-list* #P"/usr/share/applications/google-chrome.desktop"))
  (format t "entry count: ~S~%" (length *entry-list*))

  (setf *entry-list*
        (add-to-entry-list *entry-list* #P"/usr/share/applications/emacs.desktop"))
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

  (format t "all categories: ~S~%" (find-categories *entry-list*))
  (format t "entry list /:~%")
  (dolist (entry (find-entries *entry-list* :categories '()))
    (format t "~A~%" entry))
  (format t "entry list /System/Utility/:~%")
  (dolist (entry (find-entries *entry-list* :categories '("System" "Utility")))
    (format t "~A~%" entry))

  (format t "entry list /System/:~%")
  (dolist (entry (find-entries *entry-list* :categories '("System")))
    (format t "~A~%" entry))

  (format t "grouped entrys /System: ~S~%"
          (group-by-categories (find-entries *entry-list* :categories '("System"))))

  (format t "grouped entrys /: ~S~%"
          (group-by-categories (find-entries *entry-list* :categories '("AudioVideo"))))
  (format t "menu ~S~%" (build-menu '("AudioVideo") :min-entry-in-category 3))

  (format t "menu ~S~%" (build-menu '("Network") :min-entry-in-category 5))
  (return-from test-desktop-entry)
  )
