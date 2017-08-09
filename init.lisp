;; -*-lisp-*-
;;
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload :clx)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :zpng)

;;(set-prefix-key (kbd "C-z"))
;;(clear-window-placement-rules)

(load-module "mpd")
(load-module "notifications")
(load-module "cpu")
(load-module "hostname")
(load-module "wifi")
(load-module "mem")
(load-module "net")
(load-module "screenshot")
(load-module "stumptray")
(load-module "ttf-fonts")
;;(set-font "Source Code Pro 20")
;;(set-font "*-unifont-medium-*-normal-*-16-*-*-*-*-*-*-*")
;;(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 13))
(toggle-mode-line (current-screen)
                  (current-head))
(setf *screen-mode-line-format*
      (list " %v | %c | %M | %g | %d"))
(setf *window-format* "%n %s %c %m")

;;(setf *debug-level* 1)
;;(redirect-all-output (data-dir-file "debug-output" "txt"))

(defun my-popup (mode-line button x y) 
  (echo (format nil "~A ~A ~A ~A" 
                mode-line button x y
                )))
;;(add-hook *mode-line-click-hook* 'my-popup)
(load-module "miacro-theme")
(load-module "miacro-app-menu")
(run-with-timer 0 3600 #'(lambda () 
                           (miacro-theme:display-background 
                             (miacro-theme:select-random-background "~/pictures"))))
(grename "default")
(gnewbg "emacs")
(gnewbg "chrome")
(gnewbg "[4]")
(gnewbg "[5]")
(gnewbg "[6]")
(gnewbg "[7]")
(gnewbg "[8]")
(gnewbg "[9]")
(miacro-app-menu:init-menu '(("chrome" "google-chrome-stable")
                             ("xterm" "xterm")))
(stumptray:add-mode-line-hooks)
(setf *input-window-gravity* :top)
(setf *message-window-padding* 10)
(setf *mouse-focus-policy* :sloppy)
(set-msg-border-width 2)
(set-border-color "#2aa198")
