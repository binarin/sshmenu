(in-package :ru.binarin.sshmenu)

(defmethod visible-type ((item item))
  (symbol-name (type-of item)))

(defmethod full-title ((i item) separator)
  "Возвращает полное имя элемента меню (начиная от корня)"
  (format nil
          "~{~A~^|~}"
          (cons (visible-type i)
                (cdr (reverse (iter (for cur initially i then (parent cur))
                                    (while cur)
                                    (collect (title cur))))))))

(defun raise-by-title (title)
  (let ((pid (sb-ext:run-program "/usr/bin/wmctrl" (list "-F" "-a" title)
                                 :wait t :input nil :output nil)))
    (= 0 (sb-ext:process-exit-code pid))))

(defun switch-to-virtual-desktop ()
  (awhen (setting "switch-to-desktop")
    (sb-ext:run-program "/usr/bin/wmctrl" (list "-s" (format nil "~A" it))
                        :wait nil :input nil :output nil)))

(defmethod click ((l shell))
  (unless (raise-by-title (full-title l "|"))
    (let ((cmd (start-command (terminal l) l)))
      (switch-to-virtual-desktop)
      (sb-ext:run-program (car cmd) (cdr cmd)
                          :wait nil :input nil :output nil))))

(defmethod click ((menu menu))
  (make-selector-window menu))

(defun prepare-menu (item)
      (apply 'make-instance item))

(defun read-menu (path)
  (with-open-file (f path)
    (prepare-menu
     (let ((*package* (find-package :ru.binarin.sshmenu)))
       (read f)))))

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
                                 (list entry (string-downcase
                                              (format nil "~36R" i)))))
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
          (cr (make-instance 'gtk:cell-renderer-text :xalign 1.0)))
      (gtk:tree-view-column-pack-start col cr)
      (gtk:tree-view-column-add-attribute col cr "text" 2)
      (gtk:tree-view-append-column items-list col))
    (setf (gtk:tree-view-headers-visible items-list) nil)
    items-list))


(defvar *current-menu* nil)

(defvar output *standard-output*)
(defvar event)

(defun prepare-menu-view (menu click-fn)
  (let* ((items-model (menu-entries-store menu))
         (items-list (make-menu-view items-model)))
    (gobject:g-signal-connect
     items-list "key-press-event"
     (lambda (w e)
       (declare (ignorable w e))
       (when (null (gdk:event-key-state e))
         (awhen (key-to-item-position menu (gdk:event-key-string e))
           (funcall click-fn menu (elt (entries menu) it))))))
    (gobject:g-signal-connect
     items-list "row-activated"
     (lambda (tree-view path column)
       (declare (ignorable tree-view column))
       (funcall click-fn menu
                (elt (entries menu)
                     (first (gtk:tree-path-indices path))))))
    items-list))

(defun make-quit-button ()
  (let ((quit-button (make-instance 'gtk:button :label "Quit")))
    (gobject:g-signal-connect
     quit-button "clicked"
     (lambda (w)
       (declare (ignorable w))
       (gtk:object-destroy *current-menu*)
       (setf *current-menu* nil)
       (gtk:gtk-main-quit)))
    quit-button))

(defun pack-main-container (items quit-button options-button)
  (let* ((vbox (make-instance 'gtk:v-box))
         (buttons-hbox (make-instance 'gtk:h-box :homogeneous t)))
    (gtk:box-pack-start buttons-hbox options-button)
    (gtk:box-pack-start buttons-hbox quit-button)
    (gtk:box-pack-start vbox items)
    (gtk:box-pack-start vbox buttons-hbox)
    vbox))

(defun make-selector-window (menu)
  (gtk:with-main-loop
    (let* ((window (make-instance 'gtk:gtk-window
                                  :window-position :center
                                  :title (title menu)
                                  :default-width 10
                                  :default-height 10))
           (options-button (make-instance 'gtk:button :label "Options"))
           (quit-button (make-quit-button))
           (items-list
            (prepare-menu-view
             menu
             (lambda (menu item)
               (declare (ignore menu))
               (gtk:object-destroy *current-menu*)
               (setf *current-menu* nil)
               (click item))))
           (container (pack-main-container items-list quit-button options-button)))
      (gobject:g-signal-connect
       items-list "key-press-event"
       (lambda (w e)
         (declare (ignorable w e))
         (when (string-equal (format nil "~A" #\Esc)
                             (gdk:event-key-string e))
           (when *current-menu*
             (gtk:object-destroy *current-menu*)
             (setf *current-menu* window))
           (when (parent menu)
             (click (parent menu))))))
      (when *current-menu*
        (gtk:object-destroy *current-menu*))
      (setf *current-menu* window)
      (gtk:container-add window container)
      (gtk:widget-show window))))

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

(defun run ()
  (global-bind)
  (gtk:join-gtk-main))

(defun key-to-item-position (menu key)
  (awhen (parse-integer key :junk-allowed t :radix 36)
    (when (and (<= 1 it 35)
               (<= 1 it (length (entries menu))))
      (- it 1))))
