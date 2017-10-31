;;;; app-menu.lisp

(in-package #:desktop-entry)

;;; "app-menu" goes here. Hacks and glory await!
(export '(show-menu load-menu-file))

(defvar *app-menu* '() 
  "*app-menu* can be a list of values or an alist. If it's an alist, 
the CAR of each element is displayed in the menu. What is displayed 
as menu items must be strings.")

(defvar *main-categories* 
  '("AudioVideo"
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
    "Utility"
    "Others"))
(defvar *main-scetion* "Desktop Entry")
(defvar *entry-paths* 
  '(#P"/usr/share/applications"
    #P"~/.local/share/applications"))
(defvar *entry-list* '())

;;"reference: https://developer.gnome.org/desktop-entry-spec/"
(defclass desktop-entry ()
  ((name :initarg :name 
      :initform (error "Must supply a entry name")
      :accessor name)
   (exec :initarg :exec 
      :initform (error "Must supply a exec command")
      :accessor exec)
   (categories :initarg :categories 
      :initform '()
      :accessor categories)
   (terminal :initarg :terminal 
      :initform nil
      :accessor terminal)))

(defgeneric init-entry (entry path)
  (:documentation "init entry from a .desktop file"))

(defmethod init-entry ((entry desktop-entry) (path pathname))
  (flet 
    ((get-option (config entry-name &optional (type nil))
      (if (py-configparser:has-option-p config *main-scetion* entry-name)
        (py-configparser:get-option 
          config *main-scetion* entry-name :type type)
        nil)))
    (let* ((config (py-configparser:read-files 
                      (py-configparser:make-config) (list path)))
            (name (get-option config "Name"))
            (exec (get-option config "Exec"))
            (categories (get-option config "Categories"))
            (terminal (get-option config "Terminal" :boolean)))
      (when (string= name "") (setf name nil))
      (when (string= exec "") (setf exec nil))
      (when (and (stringp categories) (> (length categories) 0))
        (setf categories (split-string categories ";")))
      (if (and name exec)
        (with-accessors ((entry-name name) 
                         (entry-exec exec)
                         (entry-categories categories)
                         (entry-terminal terminal)) 
          entry
          (setf entry-name name)
          (setf entry-exec exec)
          (setf entry-categories categories)
          (setf entry-terminal terminal)
          entry)
        nil))))

(defun get-entry-from-desktop-file (filename)
  (let* ((entry (make-instance 'desktop-entry :name nil :exec nil))i
         (entry (init-entry entry filename)))
    entry))

(defun list-entry-files (path)
    (remove-if-not (lambda (file)
                      (search "desktop"
                        (file-namestring file)))
                    (stumpwm:list-directory path)))

(defun load-file-content (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defgeneric add-to-entry-list (entry)
  (:documentation "add an entry to *entry-list*"))

(defmethod add-to-entry-list ((entry desktop-entry))
  (setf *entry-list* (append *entry-list* (list entry))))

(defmethod add-to-entry-list ((entry pathname))
  (let ((entry (get-entry-from-desktop-file entry)))
    (when entry (add-to-entry-list entry))))

(defun init-entry-list (&optional (paths *entry-paths*))
  (setf *entry-list* nil)
  (dolist (entry-path paths)
    (dolist (entry-file (list-entry-files entry-path))
      (add-to-entry-list entry-file))))

(defun get-menu-by-categories (categories)
  (when (stringp categories) 
    (setf categories (split-string categories ";")))
  (flet (
    (entry-in-catetory (entry category)
      (dolist (entry-category (categories entry))
        (when (string= entry-category category)
          (return T))
        nil)))
      
    (loop for entry in *entry-list*
      for category in categories
      when (entry-in-catetory entry category)
      collect (name entry))))
