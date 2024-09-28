(defsystem "flx"
  :version "0.1.0"
  :author "Jen-Chieh Shen"
  :license "MIT"
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Rewrite emacs-flx in Common Lisp")
