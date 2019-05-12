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
(ql:quickload :py-configparser)

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
(load-module "command-history")
(load-module "end-session")
(load-module "scrot")

;;(set-font "Source Code Pro 20")
;;(set-font "*-unifont-medium-*-normal-*-16-*-*-*-*-*-*-*")
;;(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 13))
(setf *window-format* "%n %s %c %m")

;;(setf *debug-level* 1)
;;(redirect-all-output (data-dir-file ".debug/debug-output" "txt"))

(defun my-popup (mode-line button x y)
  (echo (format nil "~A ~A ~A ~A"
                mode-line button x y)))
;;(add-hook *mode-line-click-hook* 'my-popup)
(load-module "miacro-theme")
(load-module "desktop-entry")
(defcommand display-random-background () ()
            (miacro-theme:display-background
             (miacro-theme:select-random-background "~/pictures")))

(defun screenshot-filename ()
  (format nil "~~/screenshot/stumpwm-screenshot-~a.png"
          (string-trim '(#\Newline)
                       (run-shell-command "date +%Y%m%d%H%M%S" t))))
(defcommand screenshot-auto () ()
            (stumpwm:run-commands
             (format nil "screenshot ~a" (screenshot-filename))))
(defcommand screenshot-window-auto () ()
            (stumpwm:run-commands
             (format nil "screenshot-window ~a" (screenshot-filename))))
(run-with-timer 0 3600 #'display-random-background)
(grename "1")
(gnewbg "2")
(gnewbg "3")
(gnewbg "4")
(gnewbg "5")
(gnewbg "6")
(gnewbg "7")
(gnewbg "8")
(gnewbg "9")
(stumptray:add-mode-line-hooks)
(setf *input-window-gravity* :top)
(setf *message-window-padding* 10)
(setf *message-window-gravity* :bottom-right)
;; (setf *mouse-focus-policy* :sloppy)
(setf *mouse-focus-policy* :click)
(setf *input-window-gravity* :bottom)
(set-msg-border-width 2)
(set-border-color "#2aa198")

(setf *mode-line-timeout* 1)
(setf stumpwm:*screen-mode-line-format*
      (list
       (miacro-theme:mode-line-unit "%n")
       "^[^7*^B%W^]"
       "^>" ; right align
       ;; (miacro-theme:mode-line-unit "%h")
       (miacro-theme:mode-line-unit "^[^5*^B%C^]")
       (miacro-theme:mode-line-unit "^[^5*^B%M^]")
       (miacro-theme:mode-line-unit "^[^5*^B%l^]")
       (miacro-theme:mode-line-separator-start)
       '(:eval (string-right-trim '(#\Newline)
                (run-shell-command
                 "date +'^[^B%Y-%m-%d ^6*%H:%M:%S %a^]'" t)))
       (miacro-theme:mode-line-separator-end)
       "    "))

(toggle-mode-line (current-screen) (current-head))

(desktop-entry:init-entry-list)
(define-key *root-map* (kbd "m") "show-desktop-menu")
(desktop-entry:add-favorite-entry "Google Chrome")
(desktop-entry:add-favorite-entry "Chromium Web Browser")
(desktop-entry:add-favorite-entry "Remmina")
(desktop-entry:add-favorite-entry "GNU Image Manipulation Program")

(defun run-command-from-file (filename)
  (with-open-file (stream filename :direction :input)
    (loop for command in (read stream)
       do (run-shell-command command nil))))
(defvar command-file "~/.stumpwm.d/.initrc")
(when (probe-file command-file)
  (run-command-from-file "~/.stumpwm.d/.initrc"))

(defun shell-command-exists-p (command)
  (if (string=
        (run-shell-command
          (concatenate 'string
            "type -a "
            command
            " > /dev/null && echo -n true || echo -n false") T)
        "true")
   T
   nil))
;; C-t C-c for xterm
(when (shell-command-exists-p "urxvt")
  (define-key *root-map* (kbd "c") "exec urxvt"))
;; C-t C-e for emacs
(when (shell-command-exists-p "emcx")
  (define-key *root-map* (kbd "e") "exec emcx"))
