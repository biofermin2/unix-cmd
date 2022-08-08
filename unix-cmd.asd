(defsystem "unix-cmd"
  :version "0.1.0"
  :author "biofermin2"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "unix-cmd"))))
  :description "common lisp functions for unix command-like operations"
  :in-order-to ((test-op (test-op "unix-cmd/tests"))))

(defsystem "unix-cmd/tests"
  :author ""
  :license ""
  :depends-on ("unix-cmd"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for unix-cmd"
  :perform (test-op (op c) (symbol-call :rove :run c)))
