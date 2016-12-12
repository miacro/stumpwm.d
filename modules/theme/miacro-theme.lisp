;;;; miacro-theme.lisp

(in-package #:miacro-theme)

;;; "miacro-theme" goes here. Hacks and glory await!

;; -*-lisp-*-
;;
;;; The following lines added by ql:add-to-init-file:
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
