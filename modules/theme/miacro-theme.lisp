;;;; miacro-theme.lisp

(in-package #:miacro-theme)

;;; "miacro-theme" goes here. Hacks and glory await!

;; -*-lisp-*-
;;
;;; The following lines added by ql:add-to-init-file:


(setf *mode-line-separator* "^[^B««^]")
(setf *mode-line-separator-start* "^[^7*^B[^]")
(setf *mode-line-separator-end* "^[^7*^B]^]")

(defun mode-line-separator-start ()
  *mode-line-separator-start*)
(defun mode-line-separator-end ()
  *mode-line-separator-end*)
(defun mode-line-unit (unit-string &optional &key (start T) (end T))
  (let ((start-string *mode-line-separator-start*) 
        (end-string *mode-line-separator-end*))
    (if (not start) (setf start-string ""))
    (if (not end) (setf end-string ""))
    (concatenate 'string 
      start-string unit-string end-string)))
            
(defun select-random-background (image-dir)
  "Select a random image from image-dir"
  (flet ((get-file-list-by-type
           (type) 
           (directory (concatenate 'string image-dir "/**/*." type))))
    (let ((file-list (append (get-file-list-by-type "jpg")
                             (get-file-list-by-type "png"))))
      (namestring 
        (nth 
          (random 
            (length file-list) 
            (make-random-state t)) file-list)))))
(defun display-background (image-path)
  "Select display an image"
  (run-shell-command (concatenate 'string "display -window root " image-path)))
