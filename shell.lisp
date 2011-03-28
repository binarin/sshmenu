(in-package :ru.binarin.sshmenu)

(defclass shell (item)
  ((mux :initarg :mux :accessor mux :initform (setting "default-mux"))
   (terminal :initarg :terminal :accessor terminal :initform (setting "default-terminal"))))

(defmethod initialize-instance :after ((shell shell) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (mux shell) (setting (mux shell))))

(defclass local-shell (shell)
  ())

(defclass remote-shell (shell)
  ((host :initarg :host :accessor host)
   (login :initarg :login :accessor login)
   (bgcolor :initarg :bgcolor :accessor bgcolor)
   (tile :initarg :tile :accessor tile)
   (terminal :initarg :terminal :accessor terminal :initform 'rxvt-unicode)
   (rsh :initarg :rsh :accessor rsh :initform 'ssh)
   (mux :initarg :mux :accessor mux :initform 'screen)))

(defmethod visible-type ((item local-shell))
  "LOCAL")

(defmethod visible-type ((item remote-shell))
  "SSH")

(defmethod rsh ((shell local-shell))
  (declare (ignore shell))
  nil)

(defmethod start-command ((shell local-shell))
  (start-terminal-command (terminal shell)
                          (full-title shell "|")
                          (aif (mux shell)
                               (start-mux-command it
                                                  (start-rsh-command (rsh shell)))
                               (start-rsh-command (rsh shell)))))
