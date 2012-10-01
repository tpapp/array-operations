;;;; package.lisp

(defpackage #:array-operations
  (:use #:cl #:alexandria #:anaphora #:let-plus)
  (:nicknames #:ao)
  (:shadow #:flatten)
  (:export ; utilities
   #:walk-subscripts)
  (:export ; displacement
   #:displace
   #:flatten
   #:split
   #:sub
   #:partition
   #:combine
   #:subvec
   #:reshape)
  (:export ; transformations
   #:generate*
   #:generate
   #:permutation-repeated-index
   #:permutation-invalid-index
   #:valid-permutation?
   #:complement-permutation
   #:permute
   #:each*
   #:each
   #:margin*
   #:margin))
