(defsystem "flx"
  :version "0.1.0"
  :author "Jen-Chieh Shen"
  :license "MIT"
  :components ((:file "flx"))
  :description "Rewrite emacs-flx in Common Lisp")
  
(defsystem "flx/tests"
  :author "Jen-Chieh Shen"
  :license "MIT"
  :depends-on ("flx"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for flx"
  :perform (test-op (op c) (symbol-call :rove :run c)))
