(in-package :ru.binarin.sshmenu)

(defclass terminal-mux ()
  ())

(defclass screen-mux (terminal-mux)
  ((name :initarg :name :reader mux-name)
   (exec-file :initarg :exec-file :reader exec-file :initform "/usr/bin/screen")))

(defclass tmux-mux (terminal-mux)
  ((name :initarg :name :reader mux-name)
   (exec-file :initarg :exec-file :reader exec-file :initform "/usr/bin/tmux")))

(defmethod start-command ((mux screen-mux) (shell shell))
  (list (exec-file mux) "-D" "-RR" "-h" "20000" "-S" (mux-name mux)))

(defmethod start-command ((mux tmux-mux) (shell shell))
;;  bash -c 'tmux attach -t binarin -d || tmux new -s binarin'
  (list "/bin/sh" "-c"
        (concatenate 'string
                     (exec-file mux)
                     " attach -t " (mux-name mux) " -d || "
                     (exec-file mux) " new -s " (mux-name mux))))



