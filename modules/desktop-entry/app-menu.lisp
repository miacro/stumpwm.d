;;;; app-menu.lisp

(in-package #:desktop-entry)

(defvar *favorite-category* "Favorite")
(defvar *main-categories* 
  (list 
    *favorite-category*
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

(defvar *entry-paths* 
  '(#P"/usr/share/applications"
    #P"~/.local/share/applications"))
(defvar *entry-list* '())

(defgeneric set-entry-favorite (entry &optional &key entry-list favorite-category)
  (:documentation "add entry as favorite"))

(defmethod set-entry-favorite ((entry-name string) 
                               &optional &key 
                               (entry-list *entry-list*)
                               (favorite-category *favorite-category*))
  (let ((entry-index (position entry-name entry-list 
                        :test (lambda (name entry) (string= name (name entry))))))
    (when entry-index 
      (add-category (nth entry-index entry-list) favorite-category))))


(defgeneric add-to-entry-list (entry)
  (:documentation "add an entry to *entry-list*"))

(defmethod add-to-entry-list ((entry desktop-entry))
  (setf *entry-list* (append *entry-list* (list entry))))

(defmethod add-to-entry-list ((entry pathname))
  (let ((entry (make-desktop-entry entry)))
    (when entry (add-to-entry-list entry))))

(defun init-entry-list (&optional (entry-paths *entry-paths*))
  (setf *entry-list* nil)
  (dolist (entry-path entry-paths)
    (dolist (entry-file (list-entry-files entry-path))
      (add-to-entry-list entry-file))))

(defun get-menu-by-categories (categories &optional &key (entry-list *entry-list*))
  (when (stringp categories) 
    (setf categories (string-split ";" categories)))
    (let ((menu nil) (entry nil))
      (loop for index from (- (length entry-list) 1) downto 0 by 1
        do (setf entry (nth index entry-list))
        when (and (not (no-display entry)) 
                  (not (only-show-in entry))
                  (or (string= "Application" (entry-type entry))
                      ;;(string= "Directory" (entry-type entry))
                      ;;(string= "Link" (entry-type entry))
                  ))
          do (dolist (category categories)
              (when (entry-in-category-p entry category)
                 (setf menu (cons (cons (name entry) index) menu)))))
      menu))

(defun filter-entry-by-categories (categories &optional &key (entry-list *entry-list*))
  (loop for entry in entry-list
    when (and (not (no-display entry)) 
              (not (only-show-in entry))
              (or (string= "Application" (entry-type entry))
                  ;;(string= "Directory" (entry-type entry))
                  ;;(string= "Link" (entry-type entry))
              )
              (block test-entry 
                (dolist (category categories)
                  (when (not (entry-in-category-p entry category))
                    (return-from test-entry nil)))
                (return-from test-entry T)))
    collect entry))

(defun get-all-categories (&optional &key
                           (entry-list *entry-list*))
  (flet ((add-category-to-list (category category-list)
            (let ((index (position category category-list :test #'string=)))
              (when (not index) (setf category-list (cons category category-list)))
              category-list)))
    (let ((category-list nil))
      (dolist (entry entry-list)
        (dolist (category (categories entry))
          (setf category-list (add-category-to-list category category-list))))
      category-list)))

(defun group-entry-by-categories (&optional &key (entry-list *entry-list*))
  (let ((grouped-entrys nil) (other-entrys nil))
    (dolist (category (get-all-categories :entry-list entry-list))
      (setf grouped-entrys (cons (cons category nil) grouped-entrys))
      (dolist (entry entry-list)
        (when (entry-in-category-p entry category)
          (setf (cdr (car grouped-entrys)) 
            (cons entry (cdr (car grouped-entrys)))))))
    (dolist (entry entry-list)
      (when (not (position entry grouped-entrys 
                    :test 
                    (lambda (a b) 
                      (position a (cdr b) :test #'eq))))
        (setf other-entrys (cons entry other-entrys))))
    (append grouped-entrys other-entrys)))

(defun build-menu2 (&optional (categories nil)
                   &key 
                   (entry-list *entry-list*) 
                   (main-categories *main-categories*))
  (when categories 
    (setf entry-list (filter-entry-by-categories categories :entry-list entry-list)))
  (let ((grouped-entrys (group-entry-by-categories :entry-list entry-list))
        (menu (cons "/" nil)))
    (dolist (category categories)
      (setf (car menu) (concatenate 'string (car menu) category "/")))
    (dolist (item grouped-entrys)
      (cond 
        ((listp item)
         (setf (cdr menu) 
            (cons (cons (car item) (car item)) (cdr menu))) 
          (format t "~A~%" menu))
        (T 
         (setf (cdr menu) (name item)))))
    menu))

(defun build-menu (&optional (categories nil)
                   &key 
                   (entry-list *entry-list*) 
                   (main-categories *main-categories*))
  (if categories
    (append (get-menu-by-categories categories :entry-list entry-list) 
            (list '(".." . :up) '("...." . nil)))
    (append (loop for category in main-categories
              collect (cons (concatenate 'string category " >>") category))
            (list '(".." . nil)))))

(stumpwm:defcommand show-menu () ()
  "show the application menu"
  (let ((stack-categories nil) (prompt "Application:"))
    (loop
      (let ((entry-index 
              (cdr (stumpwm:select-from-menu 
                      (stumpwm:current-screen) 
                      (build-menu (reverse stack-categories))
                      prompt))))
        (cond 
          ((not entry-index) (return))
          ((numberp entry-index)
                (stumpwm:run-shell-command 
                  (command-line 
                    (nth entry-index *entry-list*))) (return))
          ((stringp entry-index)
            (push entry-index stack-categories))
          ((eq entry-index :up)
            (pop stack-categories)))))))
