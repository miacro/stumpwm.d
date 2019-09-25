;;;; scrot.lisp

(in-package #:scrot)

(defvar *image-editor* "'gthumb $f'")
(defvar *image-directory* "~/screenshot")

(defun filenamer ()
  (format nil "~a/scrot-~a.png"
          *image-directory*
          (string-trim
           '(#\Newline)
           (stumpwm:run-shell-command "date +%Y%m%d%H%M%S" t))))

(defun with-editor ()
  (format nil "-e ~a" *image-editor*))

(defun with-filename ()
  (let ((filename (filenamer)))
    (stumpwm:run-shell-command
     (format nil "sh -c '[[ -d $(dirname ~a) ]] || mkdir -p $(dirname ~a)'"
             filename
             filename)
     t)
    filename))

(defun run-scrot (&optional (extra-args ""))
  (stumpwm:run-shell-command
   (format nil "sleep 0.2; scrot ~a ~a" extra-args (with-filename))
   nil))

(stumpwm:defcommand scrot ()
  ()
  (run-scrot))

(stumpwm:defcommand scrot-select ()
  ()
  (run-scrot "-s"))

(stumpwm:defcommand scrot-window ()
  ()
  (run-scrot "-u"))

(stumpwm:defcommand scrot-with-editor ()
  ()
  (run-scrot (with-editor)))

(stumpwm:defcommand scrot-select-with-editor ()
  ()
  (run-scrot (format nil "-s ~a" (with-editor))))

(stumpwm:defcommand scrot-window-with-editor ()
  ()
  (run-scrot (format nil "-u ~a" (with-editor))))
