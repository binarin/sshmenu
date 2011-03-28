(in-package :ru.binarin.sshmenu)

(defclass item ()
  ((title :initarg :title :reader title)
   (parent :initarg :parent :reader parent :initform nil)))

(defclass menu (item)
  ((entries :initarg :entries :accessor entries)
   (defaults :initarg :defaults :accessor defaults)))

(defmethod initialize-instance :after ((menu menu) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (entries menu) (mapcar (lambda (item) (apply 'make-instance item))
                               (entries menu)))
  (mapc #'(lambda (item) (setf (slot-value item 'parent) menu))
        (entries menu)))

