(in-package :ru.binarin.sshmenu)

(defparameter *settings* (make-hash-table :test 'equal))

(defun initialize-settings ()
  (setf (gethash "global-key" *settings*) "<Super>semicolon"
        (gethash "menu-file" *settings*) #p"/home/binarin/.sshmenu-cl"
        (gethash "pixmap-path" *settings*) "/home/binarin/ssh/images"
        (gethash "switch-to-desktop" *settings*) 0
        (gethash "default-terminal" *settings*) (make-instance 'rxvt-terminal)
        (gethash "default-mux" *settings*) (make-instance 'screen-mux :name "binarin")
        (gethash "screen-mux" *settings*) (make-instance 'screen-mux :name "binarin")
        (gethash "tmux-mux" *settings*) (make-instance 'tmux-mux :name "binarin")
        (gethash "default-rsh" *settings*) (make-instance 'ssh-rsh)))

(defun setting (name)
    (gethash name *settings*))
