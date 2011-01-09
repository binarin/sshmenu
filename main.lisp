(in-package :ru.binarin.sshmenu)

(defclass item ()
  ((title :initarg :title :reader title)
   (parent :initarg :parent :reader parent)))

(defclass menu (item)
  ((entries :initarg :entries :accessor entries)
   (defaults :initarg :defaults :accessor defaults)))

(defmethod initialize-instance :after ((menu menu) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (mapc #'(lambda (item) (setf (slot-value item 'parent) menu))
        (entries menu)))

(defmethod click ((menu menu))
  (let* ((output (make-string-output-stream))
         (input (make-string-input-stream
                 (format nil "~{~A ~A~%~}"
                         (iterate (for el in (entries *root-menu*))
                                  (for i from 1)
                                  (appending (list i (title el)))))))
         (proc (sb-ext:run-program
                "/usr/bin/zenity"
                (list "--list" "--column" "a,b")
                :input input :output output :wait t)))
    (if (= 0 (sb-ext:process-exit-code proc))
        (get-output-stream-string output))))

(defparameter *root-menu*
  (make-instance
   'menu :title "Root"
   :entries (list
             (make-instance
              'menu :title "Home"
              :entries (list (make-instance 'item :title "vassago")
                             (make-instance 'item :title "ishamael")))
             (make-instance 'item :title "Терминал"))))

(defparameter *menu*
   '(("terminal" :type rsh :rsh nil)
     ("home"
      ("ishamael rdp" :type rdp :host "10.8.0.1")
      ("ishamael" :type rsh
       :host "ishamael.binarin.ru" :login "binarin"
       :bgcolor "darkred" :rsh ssh :mux screen
       ))))

