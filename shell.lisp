(in-package :ru.binarin.sshmenu)

(defclass shell (item)
  ((terminal :initarg :terminal :accessor terminal :initform (setting "default-terminal"))
   (mux :initarg :mux :accessor mux :initform "default-mux")))

(defmethod initialize-instance :after ((shell shell) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (mux shell)
      (setf (mux shell) (setting (mux shell)))))

(defclass local-shell (shell)
  ())

(defclass remote-shell (shell)
  ((host :initarg :host :accessor host)
   (login :initarg :login :accessor login)
   (bgcolor :initarg :bgcolor :accessor bgcolor)
   (tile :initarg :tile :accessor tile)
   (rsh :initarg :rsh :accessor rsh :initform (setting "default-rsh"))))

(defmethod visible-type ((item local-shell))
  "LOCAL")

(defmethod visible-type ((item remote-shell))
  "SSH")

(defmethod rsh ((shell local-shell))
  (declare (ignore shell))
  nil)


