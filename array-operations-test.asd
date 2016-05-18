(asdf:defsystem #:array-operations-test
  :serial t
  :description "Unit tests for the ARRAY-OPERATIONS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :depends-on (#:array-operations       ; loads everything else
               #:clunit)
  :pathname #P"tests/"
  :components ((:file "tests"))
  :perform (test-op :after (op c)
                    (eval (read-from-string "(time (array-operations-tests::run))"))))
