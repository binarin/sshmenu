(in-package :ru.binarin.sshmenu)

(defclass rsh ()
  ())

(defclass ssh-rsh ()
  ())

(defclass mosh-rsh ()
  ())

(defmethod start-command ((rsh ssh-rsh) shell)
  (list* "ssh" "-l" (login shell) (host shell)
         (acond ((mux shell) (list* "-t" (start-command it shell)))
                (t '()))))

(defmethod start-command ((mosh mosh-rsh) shell)
  (list* "mosh"
         (format nil "~a@~a" (login shell) (host shell))
         (acond ((mux shell) (list* "--" (start-command it shell)))
                (t '()))))
