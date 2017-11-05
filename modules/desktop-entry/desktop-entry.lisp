;;;; app-menu.lisp

(in-package #:desktop-entry)

(defvar *main-section* "Desktop Entry")
;;"reference: https://developer.gnome.org/desktop-entry-spec/"
(defclass desktop-entry ()
  ((entry-type :initarg :entry-type
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

(defmethod print-object ((object desktop-entry) stream)
  (format stream "(:entry-type ~S :name ~S :categories ~S)"
    (entry-type object) (name object) (categories object)))

(defun load-desktop-file (path &optional &key (main-section *main-section*))
  (flet 
    ((get-option (config entry-name &optional (type nil))
      (if (py-configparser:has-option-p config main-section entry-name)
        (py-configparser:get-option 
          config main-section entry-name :type type)
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
      (list
        :name (if (string= name "") nil name)
        :entry-type entry-type
        :exec (if (string= exec "") nil exec)
        :path path
        :categories (if (stringp categories) (string-split ";" categories) categories)
        :no-display no-display
        :only-show-in (if (stringp only-show-in) (string-split ";" only-show-in) only-show-in)
        :terminal terminal))))

(defgeneric make-desktop-entry (path &optional &key main-section)
  (:documentation "init entry from a .desktop file"))

(defmethod make-desktop-entry ((entry-content list)
                               &optional &key (main-section *main-section*))
  (make-instance 'desktop-entry 
    :name (getf entry-content :name)
    :entry-type (getf entry-content :entry-type)
    :exec (getf entry-content :exec)
    :path (getf entry-content :path)
    :categories (getf entry-content :categories)
    :no-display (getf entry-content :no-display)
    :only-show-in (getf entry-content :only-show-in)
    :terminal (getf entry-content :terminal)))

(defmethod make-desktop-entry ((path pathname) 
                                &optional &key (main-section *main-section*))
  (make-desktop-entry (load-desktop-file path :main-section main-section)))

(defgeneric command-line (entry)
  (:documentation "get command line from an entry"))

(defmethod command-line (entry)
  (let ((exec-string (exec entry))
        (path-string (path entry)))
    (concatenate 'string path-string
      (string-replace-all "%f|%F|%u|%U|%d|%D|%n|%N|%i|%c|%k|%v|%m" 
        exec-string ""))))

(defgeneric add-category (entry category)
  (:documentation "add a category to an entry"))

(defmethod add-category ((entry desktop-entry) (category string))
  (with-accessors ((categories categories)) entry
    (when 
      (not (position category categories :test #'string=))
      (setf categories (cons category categories)))))

(defun entry-in-category-p (entry category)
  (dolist (entry-category (categories entry))
    (when (string= entry-category category)
      (return T))
    nil))
