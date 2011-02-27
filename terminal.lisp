(in-package :ru.binarin.sshmenu)

(defclass terminal-emulator ()
  ())

(defgeneric start-terminal-command (terminal-emulator title nested-command)
  (:documentation
   "Return command and its args for running terminal with given title
   executing nested-command. Both nested-command and return value are
   lists where first element is command and rest is args for it."))

(defclass rxvt-terminal ()
  ((pixmap :initarg :pixmap :reader pixmap :initform nil)))

(defmethod start-terminal-command ((rxvt rxvt-terminal) title nested-command)
  (list* "/usr/bin/rxvt-unicode"
         "-T" title "-e" nested-command))
