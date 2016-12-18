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

(set-prefix-key (kbd "C-z"))
;;(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")
;;(set-font "Source Code Pro 20")
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
(toggle-mode-line (current-screen)
                  (current-head))
(setf *screen-mode-line-format*
      (list " %w | %c | %M | " 
            '(:eval (run-shell-command "date" t))))

;;(setf *debug-level* 1)
;;(redirect-all-output (data-dir-file "debug-output" "txt"))

(defun my-popup (mode-line button x y) 
  (echo (format nil "~A ~A ~A ~A" 
                mode-line button x y
                )))
(add-hook *mode-line-click-hook* 'my-popup)
(load-module "miacro-theme")
(load-module "miacro-app-menu")
(run-with-timer 0 3600 #'(lambda () 
                           (miacro-theme:display-background 
                             (miacro-theme:select-random-background "~/pictures"))))
(grename "chrome")
(gnewbg "emacs")
(gnewbg "rtorrent")
(miacro-app-menu:init-menu '(("chrome" "google-chrome-stable")
                             ("xterm" "xterm")))
(stumptray:add-mode-line-hooks)
(setf *input-window-gravity* :top)
(setf *message-window-padding* 10)
(set-msg-border-width 2)
(set-border-color "#2aa198")
