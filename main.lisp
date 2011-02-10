(in-package :ru.binarin.sshmenu)

(defclass item ()
  ((title :initarg :title :reader title)
   (parent :initarg :parent :reader parent :initform nil)))

(defclass remote-shell (item)
  ((host :initarg :host :accessor host)
   (login :initarg :login :accessor login)
   (bgcolor :initarg :bgcolor :accessor bgcolor)
   (tile :initarg :tile :accessor tile)
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
  (list* "/usr/bin/rxvt-unicode"
         "-pixmap" (concatenate 'string "/home/binarin/ssh/images/" (tile r) ";0x0+50+50:tile")
         "-T" (full-title r "|" "SSH") "-e" args))

(defmethod shell-command ((r remote-shell) (rsh (eql 'ssh)) args)
  (if args
      (list* "ssh" "-l" (login r) (host r) "-t" args)
      (list* "ssh" "-l" (login r) (host r))))

(defmethod shell-command ((r remote-shell) (mux (eql 'screen)) args)
  (list "screen" "-D" "-RR" "-h" "20000" "-S" "binarin"))

(defun raise-by-title (title)
  (let ((pid (sb-ext:run-program "/usr/bin/wmctrl" (list "-F" "-a" title)
                                 :wait t :input nil :output nil)))
    (= 0 (sb-ext:process-exit-code pid))))

(defmethod click ((r remote-shell))
  (unless (raise-by-title (full-title r "|" "SSH"))
    (let ((cmd (shell-command r (terminal r)
                              (shell-command r (rsh r)
                                             (shell-command r (mux r) nil)))))
        (sb-ext:run-program (car cmd) (cdr cmd)
                            :wait nil :input nil :output nil))))

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

(defun prepare-menu (item)
  (if (listp (second item))
      (make-instance 'menu :title (car item)
                     :entries (mapcar 'prepare-menu (cdr item)))
      (apply 'make-instance 'remote-shell :title (car item) (cdr item))))

(defun read-menu (path)
  (with-open-file (f path)
    (prepare-menu (read f))))

(defun menu-entries-store (menu)
  (let ((model (make-instance 'gtk:array-list-store)))
    ;; hotkey
    (gtk:store-add-column model "gchararray"
                          (lambda (item) (second item)))
    ;; item title
    (gtk:store-add-column model "gchararray"
                          (lambda (item) (format nil "~A" (title (first item)))))
    ;; arrow for submenus
    (gtk:store-add-column model "gchararray"
                          (lambda (item) (if (typep (first item) 'menu) "▶" "")))

    (iterate (for i from 1)
             (for entry in (entries menu))
             (gtk:store-add-item model
                                 (list entry (format nil "~A" i))))
    model))

(defun make-menu-view (model)
  (let* ((items-list (make-instance 'gtk:tree-view :model model)))
    (let ((col (make-instance 'gtk:tree-view-column :title "Hotkey"))
          (cr (make-instance 'gtk:cell-renderer-text)))
      (gtk:tree-view-column-pack-start col cr)
      (gtk:tree-view-column-add-attribute col cr "text" 0)
      (gtk:tree-view-append-column items-list col))
    (let ((col (make-instance 'gtk:tree-view-column :title "Title"))
          (cr (make-instance 'gtk:cell-renderer-text)))
      (gtk:tree-view-column-pack-start col cr)
      (gtk:tree-view-column-add-attribute col cr "text" 1)
      (gtk:tree-view-append-column items-list col))
    (let ((col (make-instance 'gtk:tree-view-column :title "Submenu"))
          (cr (make-instance 'gtk:cell-renderer-text)))
      (gtk:tree-view-column-pack-start col cr)
      (gtk:tree-view-column-add-attribute col cr "text" 2)
      (gtk:tree-view-append-column items-list col))
    (setf (gtk:tree-view-headers-visible items-list) nil)
    items-list))


(defvar *current-menu* nil)


(defun make-selector-window (menu)
  (let ((output *standard-output*))
    (declare (ignorable output))
    (gtk:with-main-loop
      (let* ((window (make-instance 'gtk:gtk-window
                                    :window-position :center
                                    :title (title menu)
                                    :default-width 10
                                    :default-height 10))
             (items-model (menu-entries-store menu))
             (items-list (make-menu-view items-model)))
        (when *current-menu*
          (gtk:object-destroy *current-menu*))
        (setf *current-menu* window)
        (gtk:container-add window items-list)
        (gobject:g-signal-connect
         items-list "row-activated"
         (lambda (tree-view path column)
           (declare (ignorable tree-view column))
           (format output "~A"
                   (gtk:tree-path-indices path))))
        (gtk:widget-show window)))))

(defun select-from-list (m)
  (gtk:with-main-loop
    (let ((window (make-instance 'gtk:gtk-window
                                 :title (title m)
                                 :type :toplevel)))
      (gtk:widget-show window))))

