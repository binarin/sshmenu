(defsystem sshmenu
  :version "0.1"
  :license "GNU GPL v3"
  :depends-on ("anaphora" "iterate")
  :components
  ((:file "packages")
   (:file "main" :depends-on ("packages"))))