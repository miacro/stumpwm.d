;;;; app-menu.lisp

(in-package #:desktop-entry)

;;; "app-menu" goes here. Hacks and glory await!
(export '(show-menu load-menu-file))

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

(defun string-split (regex target-string)
  (cl-ppcre:split regex target-string))

(defun string-replace-all (regex target-string replacement) 
  (cl-ppcre:regex-replace-all regex target-string replacement))

(defun list-directory (path)
  (stumpwm:list-directory path))

;;"reference: https://developer.gnome.org/desktop-entry-spec/"
(defclass desktop-entry ()
  (
   (entry-type :initarg :entry-type
      :initform (error "Must supply a entry type")
      :accessor entry-type)
   (name :initarg :name 
      :initform (error "Must supply a entry name")
      :accessor name)
   (exec :initarg :exec 
      :initform (error "Must supply a exec command")
      :accessor exec)
   (path :initarg :path
      :initform nil
      :accessor path)
   (categories :initarg :categories 
      :initform '()
      :accessor categories)
   (no-display :initarg :no-display
      :initform nil
      :accessor no-display)
   (only-show-in :initarg :only-show-in
      :initform nil
      :accessor only-show-in)
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
            (entry-type (get-option config "Type"))
            (exec (get-option config "Exec"))
            (path (get-option config "Path"))
            (categories (get-option config "Categories"))
            (no-display (get-option config "NoDisplay" :boolean))
            (only-show-in (get-option config "OnlyShowIn"))
            (terminal (get-option config "Terminal" :boolean)))
      (when (string= name "") (setf name nil))
      (when (string= exec "") (setf exec nil))
      (when (and (stringp categories))
        (setf categories (string-split ";" categories)))
      (when (and (stringp only-show-in))
        (setf only-show-in (string-split ";" only-show-in)))
      (if (and name exec)
        (with-accessors ((entry-name name) 
                         (entry-entry-type entry-type)
                         (entry-exec exec)
                         (entry-path path)
                         (entry-categories categories)
                         (entry-no-display no-display)
                         (entry-only-show-in only-show-in)
                         (entry-terminal terminal)) 
          entry
          (setf entry-name name)
          (setf entry-entry-type entry-type)
          (setf entry-exec exec)
          (setf entry-path path)
          (setf entry-categories categories)
          (setf entry-no-display no-display)
          (setf entry-only-show-in only-show-in)
          (setf entry-terminal terminal)
          entry)
        nil))))

(defgeneric command-line (entry)
  (:documentation "get command line from an entry"))

(defmethod command-line (entry)
  (let ((exec-string (exec entry))
        (path-string (path entry)))
    (concatenate 'string path-string
      (string-replace-all "%f|%F|%u|%U|%d|%D|%n|%N|%i|%c|%k|%v|%m" 
        exec-string ""))))

(defun get-entry-from-desktop-file (filename)
  (let* ((entry (make-instance 'desktop-entry :name nil :exec nil :entry-type nil))i
         (entry (init-entry entry filename)))
    entry))

(defun list-entry-files (path)
    (remove-if-not (lambda (file)
                      (search "desktop"
                        (file-namestring file)))
                    (list-directory path)))

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
    (setf categories (string-split ";" categories)))
  (flet (
    (entry-in-catetory (entry category)
      (dolist (entry-category (categories entry))
        (when (string= entry-category category)
          (return T))
        nil)))
    (let ((menu nil) (entry nil))
      (loop for index from (- (length *entry-list*) 1) downto 0 by 1
        do (setf entry (nth index *entry-list*))
        when (and (not (no-display entry)) 
                  (not (only-show-in entry))
                  (or (string= "Application" (entry-type entry))
                      ;;(string= "Directory" (entry-type entry))
                      ;;(string= "Link" (entry-type entry))
                  ))
          do (dolist (category categories)
              (when (entry-in-catetory entry category)
                 (setf menu (cons (cons (name entry) index) menu)))))
      menu)))

(stumpwm:defcommand show-menu () ()
  "show the application menu"
  (let ((category 
          (stumpwm:select-from-menu (stumpwm:current-screen) *main-categories*)))
    (let ((entry-index 
            (cdr 
              (stumpwm:select-from-menu 
                (stumpwm:current-screen) 
                (get-menu-by-categories category)))))
      (when (numberp entry-index)
        (stumpwm:run-shell-command 
          (command-line (nth entry-index *entry-list*)))))))
