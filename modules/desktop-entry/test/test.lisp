(require :desktop-entry)
(require :fiveam)
(in-package #:desktop-entry)
(defparameter *true-value-plist*
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
(defconstant *true-value-entry*
  (loop for item in *true-value-plist*
     collect (make-desktop-entry item)))

(block test-desktop-entry
  (fiveam:def-suite test-desktop-entry-suite)
  (fiveam:in-suite test-desktop-entry-suite)
  (fiveam:test init-desktop-entry
    (fiveam:is (equalp (load-desktop-file "google-chrome.desktop")
                       (first *true-value-plist*)))
    (fiveam:is (equalp (load-desktop-file "emacs.desktop")
                       (second *true-value-plist*)))
    (fiveam:is (desktop-entry-equal
                (make-desktop-entry #P"google-chrome.desktop")
                (first *true-value-entry*)))
    (fiveam:is (desktop-entry-equal
                (make-desktop-entry #P"emacs.desktop")
                (second *true-value-entry*))))

  (fiveam:test test-add-to-entry-list
    (let* ((entry-list nil)
           (entry-list (add-to-entry-list
                        entry-list
                        (make-desktop-entry #P"google-chrome.desktop")))
           (entry-list-2 (add-to-entry-list
                          entry-list
                          (make-desktop-entry #P"emacs.desktop")))
           (entry-list-3 (add-to-entry-list
                          entry-list-2
                          (make-desktop-entry #P"google-chrome.desktop"))))
      (fiveam:is (and (= 1 (length entry-list))
                      (eq 'desktop-entry
                          (type-of (first entry-list)))
                      (desktop-entry-equal
                       (first *true-value-entry*)
                       (first entry-list))))
      (fiveam:is (and (= 2 (length entry-list-2))
                      (desktop-entry-equal
                       (second *true-value-entry*)
                       (second entry-list-2))))
      (fiveam:is (= 2 (length entry-list-3)))))

  (fiveam:run! 'test-desktop-entry-suite)
  (return-from test-desktop-entry)

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
