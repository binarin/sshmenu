(in-package :ru.binarin.sshmenu)

(defclass terminal-emulator ()
  ())

(defclass rxvt-terminal ()
  ((pixmap :initarg :pixmap :reader pixmap :initform nil)))

(defmethod start-command ((term rxvt-terminal) shell)
  (concatenate
   'list
   (list "/usr/bin/rxvt-unicode" "-T" (full-title shell "|"))
   (aif (tile shell)
        (list "-pixmap" (concatenate 'string (setting "pixmap-path") "/" it ";0x0+0+0:op=tile")))
   (acond ((rsh shell) (list* "-e" (start-command it shell)))
          ((mux shell) (list* "-e" (start-command it shell)))
          (t '()))))
