;;;; app-menu.lisp

(in-package #:desktop-entry)

(defgeneric add-to-entry-list (entry-list entry)
  (:documentation "add an entry to entry-list"))

(defmethod add-to-entry-list ((entry-list list) (entry desktop-entry))
  (if (position entry entry-list :test #'desktop-entry-equal)
    entry-list
    (append entry-list (list entry))))

(defmethod add-to-entry-list ((entry-list list) (entry pathname))
  (let ((entry (make-desktop-entry entry)))
    (if entry (add-to-entry-list entry-list entry)
      entry-list)))

(defun filter-entry-list (entry-list &key categories)
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

(defun get-all-categories (entry-list)
  (flet ((add-category-to-list (category category-list)
            (let ((index (position category category-list :test #'string=)))
              (when (not index) (setf category-list (cons category category-list)))
              category-list)))
    (let ((category-list nil))
      (dolist (entry entry-list)
        (dolist (category (categories entry))
          (setf category-list (add-category-to-list category category-list))))
      category-list)))

(defun group-by-categories (entry-list &optional &key
                           (categories nil)
                           (min-entry-in-category nil)
                           (exceptive-categories nil))
  (when (not min-entry-in-category) (setf min-entry-in-category 5))
  (when (not categories) (setf categories (get-all-categories entry-list)))
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
                    :test #'(lambda (a b) (position a (cdr b) :test #'eq))))
        (setf other-entrys (cons entry other-entrys))))
    (append grouped-entrys other-entrys)))
