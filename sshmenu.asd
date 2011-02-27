(defsystem sshmenu
  :version "0.1"
  :license "GNU GPL v3"
  :depends-on ("anaphora" "iterate" "cl-gtk2-gtk" "keybinder")
  :components
  ((:file "packages")
   (:file "terminal" :depends-on ("packages"))
   (:file "main" :depends-on ("packages" "terminal"))))
