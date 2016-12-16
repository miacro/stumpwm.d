;;;; miacro-app-menu.lisp

(in-package #:miacro-app-menu)

;;; "miacro-app-menu" goes here. Hacks and glory await!

;; -*-lisp-*-
;;
;;; The following lines added by ql:add-to-init-file:
(defparameter *app-menu* nil)
(defun init-menu (menu-list) (setf *app-menu* menu-list))

(defcommand show-menu () ()
  "show app menu"
  (labels ((pick (options)
                 (let ((selection (stumpwm::select-from-menu
                                    (current-screen) options "")))
                   (cond
                     ((null selection)
                      (throw 'stumpwm::error "Abort."))
                     ((stringp (second selection))
                      (second selection))
                     (t
                       (pick (cdr selection)))))))
    (let ((choice (pick *app-menu*)))
      (run-shell-command choice))))
