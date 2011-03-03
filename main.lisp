(in-package :ru.binarin.sshmenu)

(defclass item ()
  ((title :initarg :title :reader title)
   (parent :initarg :parent :reader parent :initform nil)))

(defclass local-shell (item)
  ())

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
  (setf (entries menu) (mapcar (lambda (item) (apply 'make-instance item))
                               (entries menu)))
  (mapc #'(lambda (item) (setf (slot-value item 'parent) menu))
        (entries menu)))

(defmethod visible-type ((item item))
  (symbol-name (type-of item)))

(defmethod visible-type ((item local-shell))
  "LOCAL")

(defmethod visible-type ((item remote-shell))
  "SSH")

(defmethod full-title ((i item) separator)
  "Возвращает полное имя элемента меню (начиная от корня)"
  (format nil
          "~{~A~^|~}"
          (cons (visible-type i)
                (cdr (reverse (iter (for cur initially i then (parent cur))
                                    (while cur)
                                    (collect (title cur))))))))

(defmethod shell-command ((r remote-shell) (term (eql 'rxvt-unicode)) args)
  (declare (ignore term))
  (list* "/usr/bin/rxvt-unicode"
         "-pixmap" (concatenate 'string "/home/binarin/ssh/images/" (tile r) ";0x0+50+50:tile")
         "-T" (full-title r "|") "-e" args))

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

(defmethod click ((l local-shell))
  (unless (raise-by-title (full-title l "|"))
    (let ((cmd (start-terminal-command (make-instance 'rxvt-terminal)
                                       (full-title l "|")
                                       '("screen" "-D" "-RR" "-h" "20000"
                                         "-S" "binarin"))))
        (sb-ext:run-program (car cmd) (cdr cmd)
                            :wait nil :input nil :output nil))))

(defmethod click ((r remote-shell))
  (unless (raise-by-title (full-title r "|"))
    (let ((cmd (shell-command r (terminal r)
                              (shell-command r (rsh r)
                                             (shell-command r (mux r) nil)))))
        (sb-ext:run-program (car cmd) (cdr cmd)
                            :wait nil :input nil :output nil))))

(defmethod click ((menu menu))
  (make-selector-window menu))

(defun prepare-menu (item)
      (apply 'make-instance item))

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
           (gtk:object-destroy *current-menu*)
           (setf *current-menu* nil)
           (click (elt (entries menu)
                       (first (gtk:tree-path-indices path))))))
        (gtk:widget-show window)))))

(defun select-from-list (m)
  (gtk:with-main-loop
    (let ((window (make-instance 'gtk:gtk-window
                                 :title (title m)
                                 :type :toplevel)))
      (gtk:widget-show window))))

(defun global-bind ()
  (initialize-settings)
  (let ((output *standard-output*))
    (declare (ignorable output))
    (gtk:within-main-loop
      (keybinder:init)
      (keybinder:bind
       (setting "global-key")
       (lambda (keystring)
         (declare (ignorable keystring))
         (let ((menu (read-menu (setting "menu-file"))))
           (click menu)))))))

