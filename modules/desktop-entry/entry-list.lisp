;;;; app-menu.lisp

(in-package #:desktop-entry)

(defgeneric add-to-entry-list (entry-list entry)
  (:documentation "add an entry to entry-list"))

(defmethod add-to-entry-list ((entry-list list) (entry desktop-entry))
  (if (position entry entry-list :test #'desktop-entry-equalp)
      entry-list
      (append entry-list (list entry))))

(defmethod add-to-entry-list ((entry-list list) (entry pathname))
  (let ((entry (make-desktop-entry entry)))
    (if entry (add-to-entry-list entry-list entry)
        entry-list)))

(defun find-entries (entry-list &optional &key (test #'(lambda (entry) nil)))
  (loop for entry in entry-list
     when (funcall test entry)
     collect entry))

(defun find-categories (entry-list &optional &key (test #'(lambda (entry) T)))
  (flet ((add-category-to-list (category category-list)
           (let ((index (position category category-list :test #'string=)))
             (when (not index)
               (setf category-list (nconc category-list (list category))))
             category-list)))
    (let ((category-list nil))
      (dolist (entry entry-list)
        (when (funcall test entry)
          (dolist (category (categories entry))
            (setf category-list
                  (add-category-to-list category category-list)))))
      category-list)))

(defun group-by-categories (entry-list &optional &key
                                                   (categories nil)
                                                   (min-entry-in-category nil)
                                                   (exceptive-categories nil))
  (when (not min-entry-in-category) (setf min-entry-in-category 5))
  (when (not categories) (setf categories (find-categories entry-list)))
  (let ((grouped-entrys nil) (other-entrys nil))
    (dolist (category categories)
      (setf grouped-entrys (cons (cons category nil) grouped-entrys))
      (dolist (entry entry-list)
        (when (and (entry-in-categories-p entry (list category))
                   (not (position category
                                  exceptive-categories
                                  :test #'string=)))
          (setf (cdr (car grouped-entrys))
                (cons entry (cdr (car grouped-entrys)))))))
    (setf grouped-entrys
          (loop for item in grouped-entrys
             when (>= (length (cdr item)) min-entry-in-category)
             collect item))
    (dolist (entry entry-list)
      (when (not (position entry grouped-entrys
                           :test
                           #'(lambda (a b) (position a (cdr b) :test #'eq))))
        (setf other-entrys (cons entry other-entrys))))
    (append grouped-entrys other-entrys)))
