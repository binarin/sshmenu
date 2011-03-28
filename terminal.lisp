(in-package :ru.binarin.sshmenu)

(defclass terminal-emulator ()
  ())

(defclass rxvt-terminal ()
  ((pixmap :initarg :pixmap :reader pixmap :initform nil)))

(defmethod start-command ((term rxvt-terminal) shell)
  (list* "/usr/bin/rxvt-unicode"
         "-T" (full-title shell "|")
         (acond ((rsh shell) (list* "-e" (start-command it shell)))
                ((mux shell) (list* "-e" (start-command it shell)))
                (t '()))))

