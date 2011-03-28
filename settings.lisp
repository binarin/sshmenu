(in-package :ru.binarin.sshmenu)

(defparameter *settings* (make-hash-table :test 'equal))

(defun initialize-settings ()
  (setf (gethash "global-key" *settings*) "<Super>bracketright"
        (gethash "menu-file" *settings*) #p"/home/binarin/.sshmenu-cl"
        (gethash "switch-to-desktop" *settings*) 0
        (gethash "default-terminal" *settings*) (make-instance 'rxvt-terminal)
        (gethash "default-mux" *settings*) (make-instance 'screen-mux :name "binarin")
        (gethash "screen-mux" *settings*) (make-instance 'screen-mux :name "binarin")
        (gethash "tmux-mux" *settings*) (make-instance 'tmux-mux :name "binarin")))

(defun setting (name)
    (gethash name *settings*))
