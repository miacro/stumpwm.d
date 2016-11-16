;; -*-lisp-*-
;;

(stumpwm:set-prefix-key (kbd "C-z"))
;; Message window font
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")
(clear-window-placement-rules)

(stumpwm:load-module "mpd")
(stumpwm:load-module "notifications")
(stumpwm:load-module "cpu")
(stumpwm:load-module "hostname")
(stumpwm:load-module "wifi")
(stumpwm:load-module "app-menu")
(stumpwm:load-module "mem")
(stumpwm:load-module "net")
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))
(setf stumpwm:*screen-mode-line-format*
      (list "%w | %c | %m | %h | %M | %I |" 
            '(:eval (stumpwm:run-shell-command "date" t))))

;;(setf *debug-level* 1)
;;(redirect-all-output (data-dir-file "debug-output" "txt"))

(defun my-popup (mode-line button x y) 
  (echo (format nil "~A ~A ~A ~A ~% ~A ~% ~A ~A" 
                mode-line button x y
                )))
(stumpwm:add-hook stumpwm:*mode-line-click-hook* 'my-popup)
