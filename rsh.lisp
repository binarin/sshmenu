(in-package :ru.binarin.sshmenu)

(defclass rsh ()
  ())

(defclass ssh-rsh ()
  ())

(defmethod start-command ((rsh ssh-rsh) shell)
  (list* "ssh" "-l" (login shell) (host shell)
         (acond ((mux shell) (list* "-t" (start-command it shell)))
                (t '()))))

