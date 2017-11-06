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

(defun group-entry-by-categories (&optional &key
                                  (categories nil)
                                  (min-entry-in-category nil)
                                  (exceptive-categories nil)
                                  (entry-list *entry-list*))
  (when (not min-entry-in-category) (setf min-entry-in-category 2))
  (when (not categories) (setf categories (get-all-categories :entry-list entry-list)))
  (let ((grouped-entrys nil) (other-entrys nil))
    (dolist (category categories)
      (setf grouped-entrys (cons (cons category nil) grouped-entrys))
      (dolist (entry entry-list)
        (when (and (entry-in-category-p entry category)
                   (not
                      (position category exceptive-categories :test #'string=)))
          (setf (cdr (car grouped-entrys))
            (cons entry (cdr (car grouped-entrys)))))))
    (setf grouped-entrys 
      (loop for item in grouped-entrys
        when (>= (length (cdr item)) min-entry-in-category)
          collect item))
    (dolist (entry entry-list)
      (when (not (position entry grouped-entrys
                    :test
                    (lambda (a b)
                      (position a (cdr b) :test #'eq))))
        (setf other-entrys (cons entry other-entrys))))
    (append grouped-entrys other-entrys)))

(defun build-menu (&optional (categories nil)
                   &key
                   (entry-list *entry-list*)
                   (min-entry-in-category nil)
                   (main-categories *main-categories*))
  (setf entry-list (filter-entry-by-categories categories :entry-list entry-list))
  (let ((grouped-entrys (group-entry-by-categories
                          :categories (if categories nil *main-categories*)
                          :exceptive-categories categories
                          :min-entry-in-category min-entry-in-category
                          :entry-list entry-list))
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
  (let ((stack-categories nil))
    (loop
      (let*
        ((menu (build-menu (reverse stack-categories) 
                  :min-entry-in-category (if stack-categories nil 1)))
         (menu (cdr menu))
         (menu (sort menu (lambda (x y)
                  (cond 
                    ((and (typep x 'desktop-entry)
                          (listp y))
                      nil)
                    ((and (listp x)
                          (typep y 'desktop-entry))
                     T)
                    ((and (listp x) (listp y))
                     (string< (car x) (car y))) 
                    ((and (typep x 'desktop-entry) (typep y 'desktop-entry))
                     (string< (name x) (name y)))
                    (T nil)))))
         (menu 
            (if stack-categories
              (append menu (list (cons ".." :up) (cons "...." nil)))
              (append menu (list (cons ".." nil)))))
         (prompt (let ((prompt-string "/"))
                    (dolist (category (reverse stack-categories))
                      (setf prompt-string (concatenate 'string category "/")))
                      (setf prompt-string (concatenate 'string prompt-string ":"))))
         (item (cdr (stumpwm:select-from-menu
                      (stumpwm:current-screen)
                      menu
                      prompt))))
        (cond
          ((not item) (return))
          ((typep item 'desktop-entry)
            (stumpwm:run-shell-command (command-line item))
            (return))
          ((stringp item)
            (push item stack-categories))
          ((eq item :up)
            (pop stack-categories)))))))
