(in-package :ru.binarin.sshmenu)

(defclass item ()
  ((title :initarg :title :reader title)
   (parent :initarg :parent :reader parent :initform nil)))

(defclass remote-shell (item)
  ((host :initarg :host :accessor host)
   (login :initarg :login :accessor login)
   (bgcolor :initarg :bgcolor :accessor bgcolor)
   (terminal :initarg :terminal :accessor terminal :initform 'rxvt-unicode)
   (rsh :initarg :rsh :accessor rsh :initform 'ssh)
   (mux :initarg :mux :accessor mux :initform 'screen)))

(defclass menu (item)
  ((entries :initarg :entries :accessor entries)
   (defaults :initarg :defaults :accessor defaults)))

(defmethod initialize-instance :after ((menu menu) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (mapc #'(lambda (item) (setf (slot-value item 'parent) menu))
        (entries menu)))

(defmethod full-title ((i item) separator type)
  "Возвращает полное имя элемента меню (начиная от корня)"
  (format nil
          "~{~A~^|~}"
          (cons type (cdr (reverse (iter (for cur initially i then (parent cur))
                                         (while cur)
                                         (collect (title cur))))))))

(defmethod shell-command ((r remote-shell) (term (eql 'rxvt-unicode)) args)
  (declare (ignore term))
  (list* "/usr/bin/rxvt-unicode" "-T" (full-title r "|" "SSH") "-e" args))

(defmethod shell-command ((r remote-shell) (rsh (eql 'ssh)) args)
  (if args
      (list* "ssh" (host r) "-t" args)
      (list* "ssh" (host r))))

(defmethod shell-command ((r remote-shell) (mux (eql 'screen)) args)
  (list "screen" "-D" "-RR" "-h" "20000" "-S" "binarin"))

(defmethod click ((r remote-shell))
  (let ((cmd (shell-command r (terminal r)
                            (shell-command r (rsh r)
                                           (shell-command r (mux r) nil)))))
    (sb-ext:run-program (car cmd) (cdr cmd)
                        :wait nil :input nil :output nil)))

(defmethod click ((menu menu))
  (let* ((output (make-string-output-stream))
         (input (make-string-input-stream
                 (format nil "~{~A~A~A~%~}"
                         (iterate (for el in (entries menu))
                                  (for i from 1)
                                  (appending (list i #\Tab (title el)))))))
         (proc (sb-ext:run-program
                "/usr/bin/zenity"
                (list "--list" "--column" "a")
                :input input :output output :wait t)))
    (if (= 0 (sb-ext:process-exit-code proc))
        (awhen (elt (entries menu)
                    (1- (read (make-string-input-stream (get-output-stream-string output)))))
          (click it)))))

(defparameter *root-menu*
  (make-instance
   'menu :title "Root"
   :entries (list
             (make-instance
              'menu :title "Home"
              :entries (list (make-instance 'remote-shell
                                            :title "vassago"
                                            :host "vassago.binarin.ru")
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

