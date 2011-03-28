(defsystem sshmenu
  :version "0.1"
  :license "GNU GPL v3"
  :depends-on ("anaphora" "iterate" "cl-gtk2-gtk" "keybinder")
  :components
  ((:file "packages")
   (:file "terminal" :depends-on ("packages"))
   (:file "mux" :depends-on ("packages"))
   (:file "menu" :depends-on ("packages"))
   (:file "shell" :depends-on ("packages" "menu"))
   (:file "settings" :depends-on ("packages" "mux" "terminal"))
   (:file "main" :depends-on ("packages" "terminal" "settings" "mux"))))
