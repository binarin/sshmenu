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

;; shell: term - rsh - mux
;; shell: term - rsh
;; shell: term - mux
;; shell: term

(defmethod start-command ((term rxvt-terminal) shell)
  (list* "/usr/bin/rxvt-unicode"
         "-T" (full-title shell "|")
         (acond ((rsh shell) (list* "-e" (start-command it shell)))
                ((mux shell) (list* "-e" (start-command it shell)))
                (t '()))))

(defmethod start-terminal-command ((rxvt rxvt-terminal) title nested-command)
  (list* "/usr/bin/rxvt-unicode"
         "-T" title "-e" nested-command))

