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

(defun add-to-load-path-recursive (path)
  (dolist (module-path (stumpwm::build-load-path path)) 
    (stumpwm:add-to-load-path module-path)))

(add-to-load-path-recursive "~/.stumpwm.d/stumpwm-contrib")
(add-to-load-path-recursive "~/.stumpwm.d/modules")

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
(load-module "app-menu")
;;(set-font "Source Code Pro 20")
;;(set-font "*-unifont-medium-*-normal-*-16-*-*-*-*-*-*-*")
;;(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 13))
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
(grename "1")
(gnewbg "2")
(gnewbg "3")
(gnewbg "4")
(gnewbg "5")
(gnewbg "6")
(gnewbg "7")
(gnewbg "8")
(gnewbg "9")
(miacro-app-menu:init-menu '(("chrome" "google-chrome-stable")
                             ("xterm" "xterm")))
(stumptray:add-mode-line-hooks)
(setf *input-window-gravity* :top)
(setf *message-window-padding* 10)
(setf *message-window-gravity* :bottom-right)
(setf *mouse-focus-policy* :sloppy)
(setf *input-window-gravity* :bottom)
(set-msg-border-width 2)
(set-border-color "#2aa198")

;;(setf *screen-mode-line-format*
;;      (list " %v | %c | %M | %g | %d"))
(setf *mode-line-timeout* 1)
(setf stumpwm:*screen-mode-line-format*
  (list
    (miacro-theme:mode-line-unit "%n")
    "^[^7*^B%W^]"
    "^>" ; right align
    (miacro-theme:mode-line-unit "%h")
    (miacro-theme:mode-line-unit "^[^5*^B%c%M^]")
    (miacro-theme:mode-line-unit "^[^5*^B%l^]")
    (miacro-theme:mode-line-separator-start)
    '(:eval (string-right-trim '(#\Newline) 
              (run-shell-command
                "date +'^[^B%Y-%m-%d ^6*%H:%M:%S^]'" t)))
    (miacro-theme:mode-line-separator-end)
    "   "))

(toggle-mode-line (current-screen) (current-head))

(app-menu:load-menu-file "~/.stumpwm.d/menurc")
(setf user-menu-file "~/.stumpwm.d/.menurc")
(if (probe-file user-menu-file)
  (app-menu:load-menu-file user-menu-file))

(define-key *root-map* (kbd "m") "show-menu")

(setf app-menu::*app-menu* '(("chrome" . "google-chrome-stable") ("firefox" . "firefox")))
