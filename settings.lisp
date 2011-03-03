(in-package :ru.binarin.sshmenu)

(defparameter *settings* (make-hash-table :test 'equal))

(defun initialize-settings ()
  (setf (gethash "global-key" *settings*) "<Super>bracketright"
        (gethash "menu-file" *settings*) #p"/home/binarin/.sshmenu-cl"))

(defun setting (name)
    (gethash name *settings*))
