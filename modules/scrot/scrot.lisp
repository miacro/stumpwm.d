;;;; scrot.lisp

(in-package #:scrot)

(defvar *exec-app* "gthumb")

(defun screenshot-filename ()
   (format nil "~~/screenshot/scrot-~a.png"
              (string-trim '(#\Newline)
                                      (stumpwm:run-shell-command "date +%Y%m%d%H%M%S" t))))
(defun run-scrot (&optional (args ""))
   (stumpwm:run-shell-command 
     (format nil "scrot ~a ~a" args (screenshot-filename)) t))

(stumpwm:defcommand scrot () ()
   (run-scrot))

(stumpwm:defcommand scrot-select () ()
   (run-scrot "-s"))

(stumpwm:defcommand scrot-window () ()
   (run-scrot "-u"))

(stumpwm:defcommand scrot-with-app () ()
   (run-scrot (format nil "-e ~a" *exec-app*)))

(stumpwm:defcommand scrot-select-with-app () ()
   (run-scrot (format nil "-s -e ~a" *exec-app*)))

(stumpwm:defcommand scrot-window-with-app () ()
   (run-scrot (format nil "-u -e ~a" *exec-app*)))
