(require :desktop-entry)
(require :fiveam)
(in-package #:desktop-entry)
(defconstant *desktop-entry-pathnames*
  '(#P"google-chrome.desktop"
    #P"emacs.desktop"
    #P"xterm.desktop"
    #P"i3.desktop"
    #P"libfm-pref-apps.desktop"
    #P"firefox.desktop"))
(defconstant *true-value-plist*
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
     :terminal nil)
    (:name "XTerm"
     :entry-type "Application"
     :exec "xterm"
     :path "/usr/local/bin"
     :categories ("System" "TerminalEmulator")
     :no-display nil
     :only-show-in nil
     :terminal t)
    (:name "i3"
     :entry-type "Application"
     :exec "i3"
     :path nil
     :categories nil
     :no-display t
     :only-show-in nil
     :terminal nil)
    (:name "Preferred Applications"
     :entry-type "Application"
     :exec "libfm-pref-apps"
     :path nil
     :categories ("Settings" "DesktopSettings" "X-LXDE-Settings" "GTK")
     :no-display nil
     :only-show-in ("LXDE")
     :terminal nil)
    (:name "Mozilla Firefox"
     :entry-type "Application"
     :exec "firefox %u"
     :path nil
     :categories ("Network" "WebBrowser")
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
    (loop for index from 0 below (length *desktop-entry-pathnames*)
       do (fiveam:is (equalp (load-desktop-file
                              (nth index *desktop-entry-pathnames*))
                             (nth index *true-value-plist*))))
    (loop for index from 0 below (length *desktop-entry-pathnames*)
       do (fiveam:is (desktop-entry-equal
                      (make-desktop-entry (nth index *desktop-entry-pathnames*))
                      (nth index *true-value-entry*)))))

  (fiveam:test test-add-to-entry-list-1
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

  (fiveam:test test-add-to-entry-list-2
    (let ((entry-list nil))
      (loop for index from 0 below (length *desktop-entry-pathnames*)
         do (setf entry-list (add-to-entry-list
                              entry-list
                              (nth index *desktop-entry-pathnames*)))
           (fiveam:is (= (+ index 1) (length entry-list)))
           (fiveam:is (desktop-entry-equal
                       (nth index entry-list)
                       (nth index *true-value-entry*)))
         do (setf entry-list (add-to-entry-list
                              entry-list
                              (make-desktop-entry
                               (nth index *desktop-entry-pathnames*))))
           (fiveam:is (= (+ index 1) (length entry-list)))
           (fiveam:is (desktop-entry-equal
                       (nth index entry-list)
                       (nth index *true-value-entry*))))))

  (fiveam:test test-add-category
    (let ((entry (make-desktop-entry
                  (first *desktop-entry-pathnames*))))
      (fiveam:is (equal '("Network" "WebBrowser")
                        (categories entry)))
      (add-category entry "Test")
      (fiveam:is (equal '("Network" "WebBrowser" "Test")
                        (categories entry)))
      (add-category entry "Test")
      (fiveam:is (equal '("Network" "WebBrowser" "Test")
                        (categories entry)))))

  (fiveam:test test-find-categories
    (fiveam:is (equal '("Network" "WebBrowser"
                        "Development" "TextEditor"
                        "System" "TerminalEmulator"
                        "Settings" "DesktopSettings"
                        "X-LXDE-Settings" "GTK")
                      (find-categories *true-value-entry*)))
    (fiveam:is (equal '("Network" "WebBrowser"
                        "Development" "TextEditor"
                        "System" "TerminalEmulator")
                      (find-categories
                       *true-value-entry*
                       :test #'(lambda (entry) (not (only-show-in entry)))))))

  (fiveam:run! 'test-desktop-entry-suite)
  (return-from test-desktop-entry)

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
