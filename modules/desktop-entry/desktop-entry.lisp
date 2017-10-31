;;;; app-menu.lisp

(in-package #:desktop-entry)

;;; "app-menu" goes here. Hacks and glory await!
(export '(show-menu load-menu-file))

(defvar *app-menu* nil "Where the menu structure is held")
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

(defun load-entry-config (filename)
  (let ((config (py-configparser:make-config)))
    (py-configparser:read-files config (list filename))
    config))

(setf *testfile*  
    (load-entry-config "/usr/share/applications/google-chrome.desktop"))

(setf *entry-files* 
  (list-entry-files "/usr/share/applications/"))

(setf *entry* (make-instance 'desktop-entry :name "empyt-name" :exec "empty-exec"))
(init-entry *entry* #P"/usr/share/applications/google-chrome.desktop")
