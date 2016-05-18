;;;; array-operations.asd

(asdf:defsystem #:array-operations
  :serial t
  :description "Simple array operations library for Common Lisp."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :license "Boost Software License - Version 1.0"
  :depends-on (#:alexandria
               #:anaphora
               #:let-plus
               #:trivia)
  :pathname #P"src/"
  :components ((:file "package")
               (:file "utilities")
               (:file "general")
               (:file "displacement")
               (:file "transformations")
               (:file "stack"))
  :in-order-to ((test-op (test-op #:array-operations-test))))


