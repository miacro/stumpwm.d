;;;; app-menu.lisp

(in-package #:desktop-entry)

(defvar *main-categories*
  (list
    "AudioVideo"
    "Audio"
    "Video"
    "Development"
    "Education"
    "Game"
    "Graphics"
    "Network"
    "Office"
    "Settings"
    "System"
    "Utility"))
(defvar *favorite-category* "Favorite")
(defvar *entry-paths*
  '(#P"/usr/share/applications"
    #P"~/.local/share/applications"))
(defvar *entry-list* '())
(defvar *favorite-list* '())

(defgeneric add-favorite-entry (entry)
  (:documentation "add entry as favorite"))

(defmethod add-favorite-entry ((entry desktop-entry))
  (setf *favorite-list* (add-to-entry-list *favorite-list* entry)))

(defmethod add-favorite-entry ((entry pathname))
  (setf *favorite-list* (add-to-entry-list *favorite-list* entry)))

(defmethod add-favorite-entry ((entry-name string))
  (let ((entry-index (position entry-name *entry-list*
                        :test #'(lambda (name entry) (string= name (name entry))))))
    (when entry-index
      (add-favorite-entry (nth entry-index *entry-list*)))))


(defun init-entry-list (&optional (entry-paths *entry-paths*))
  (setf *entry-list* nil)
  (dolist (entry-path entry-paths)
    (dolist (entry-file (list-entry-files entry-path))
      (setf *entry-list* 
        (add-to-entry-list *entry-list* entry-file)))))

(defun build-menu (&optional (categories nil)
                   &key
                   (entry-list *entry-list*)
                   (min-entry-in-category nil)
                   (main-categories *main-categories*))
  (setf entry-list (filter-entry-list entry-list :categories categories))
  (let ((grouped-entrys (group-by-categories entry-list
                          :categories (if categories nil *main-categories*)
                          :exceptive-categories categories
                          :min-entry-in-category min-entry-in-category))
        (menu nil))
    (dolist (item grouped-entrys)
      (cond
        ((and (listp item)
              (not (position (car item)
                              categories
                              :test #'string=)))
          (setf menu
            (cons (cons (concatenate 'string (car item) " >>")
                        (car item))
                  menu)))
        ((typep item 'desktop-entry)
          (setf menu
            (cons (cons (name item) item) menu)))))
    menu))

(stumpwm:defcommand show-menu () ()
  "show the application menu"
  (let ((stack-categories nil) (stack-type :root))
    (loop
      (let*
        ((stack-type (cond 
                        ((not stack-categories) :root)
                        ((string= (first stack-categories) *favorite-category*) :favorite)
                        (T :normal)))
         (menu (build-menu 
                  (cond 
                    ((eq stack-type :favorite) (cdr stack-categories))
                    (T stack-categories))
                  :entry-list (cond 
                                ((eq stack-type :favorite)
                                  *favorite-list*)
                                (T *entry-list*))
                  :min-entry-in-category (if (eq stack-type :root) 1 nil)))
         (menu (sort menu
                  #'(lambda (x y)
                      (format t "~A; ~A~%" x y)
                      (cond
                        ((and (typep x 'desktop-entry)
                              (stringp y))
                          nil)
                        ((and (stringp x)
                              (typep y 'desktop-entry))
                          T)
                        ((and (stringp x) (stringp y))
                          (string-lessp x y))
                        ((and (typep x 'desktop-entry) (typep y 'desktop-entry))
                          (string-lessp (name x) (name y)))
                        (T nil))) :key #'cdr))
         (menu (cond 
                  ((eq stack-type :root)
                    (cons (cons 
                            (concatenate 'string *favorite-category* " >>") 
                            *favorite-category*) 
                      menu))
                  (T menu)))
         (menu
            (if stack-categories
              (append menu (list (cons ".." :up) (cons "...." nil)))
              (append menu (list (cons ".." nil)))))
         (menu (loop for item in menu
                  collect (cons (concatenate 'string "^[^6*^b" (car item) "^]") (cdr item))))
         (prompt (let ((prompt-string "/"))
                    (dolist (category (reverse stack-categories))
                      (setf prompt-string (concatenate 'string prompt-string category "/")))
                    (setf prompt-string (concatenate 'string prompt-string ":"))))
         (item (handler-case
                  (cdr (stumpwm:select-from-menu (stumpwm:current-screen) menu prompt))
                  (error (condition) nil))))
        (cond
          ((not item) (return))
          ((typep item 'desktop-entry)
            (stumpwm:run-shell-command (command-line item))
            (return))
          ((stringp item)
            (push item stack-categories))
          ((eq item :up)
            (pop stack-categories)))))))
