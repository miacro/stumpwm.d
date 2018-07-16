;;;; app-menu.lisp

(in-package #:desktop-entry)

(defgeneric add-to-entry-list (entry-list entry)
  (:documentation "add an entry to entry-list"))

(defmethod add-to-entry-list ((entry-list list) (entry desktop-entry))
  (if (member entry entry-list :test #'desktop-entry-equalp)
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

(defun find-categories (entry-list
                        &optional
                        &key
                          (modify #'(lambda (entry) (categories entry))))
  (let ((category-list nil))
    (dolist (entry entry-list)
      (dolist (category (funcall modify entry))
        (when (not (member category category-list :test #'string=))
          (setf category-list (nconc category-list (list category))))))
    category-list))

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
                   (not (member category
                                exceptive-categories
                                :test #'string=)))
          (setf (cdr (car grouped-entrys))
                (cons entry (cdr (car grouped-entrys)))))))
    (setf grouped-entrys
          (loop for item in grouped-entrys
             when (>= (length (cdr item)) min-entry-in-category)
             collect item))
    (dolist (entry entry-list)
      (when (not (member entry grouped-entrys
                         :test
                         #'(lambda (a b) (member a (cdr b) :test #'eq))))
        (setf other-entrys (cons entry other-entrys))))
    (append grouped-entrys other-entrys)))


(defun group-entries (entry-list &optional &key (categories nil)
                                             (min-count 0))
  (when (not categories) (setf categories (find-categories entry-list)))
  (let* ((groups
          (loop for category in categories
             when (not (eq category nil))
             collect
               (cons category
                     (loop for entry in entry-list
                        when (entry-in-categories-p entry (list category))
                        collect entry))))
         (groups
          (loop for item in groups
             when (cond ((not min-count) t)
                        ((<= min-count 0) t)
                        ((<= min-count (length (cdr item))) t)
                        (t nil))
             collect item))
         (others
          (loop for entry in entry-list
             when (not
                   (member entry groups
                           :test
                           #'(lambda (a b)
                               (member a (cdr b)
                                       :test #'eq))))
             collect entry)))
    (append groups (list (cons nil others)))))
