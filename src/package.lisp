;;;; package.lisp

(defpackage #:array-operations
  (:use #:cl #:alexandria #:anaphora #:let-plus)
  (:nicknames #:aops)
  (:shadow #:flatten)
  (:export ; utilities
   #:walk-subscripts
   #:walk-subscripts-list)
  (:export ; general
   #:matrix
   #:square-matrix?
   #:square-matrix
   #:size
   #:rank
   #:dim
   #:dims
   #:nrow
   #:ncol
   #:as-array)
  (:export ; displacement
   #:displace
   #:flatten
   #:split
   #:copy-into
   #:sub
   #:partition
   #:combine
   #:subvec
   #:reshape
   #:reshape-col
   #:reshape-row)
  (:export ; transformations
   #:generate*
   #:generate
   #:permutation-repeated-index
   #:permutation-invalid-index
   #:permutation-incompatible-rank
   #:valid-permutation?
   #:complement-permutation
   #:complete-permutation
   #:invert-permutation
   #:identity-permutation?
   #:permute
   #:transpose
   #:each*
   #:each
   #:margin*
   #:margin
   #:recycle
   #:outer*
   #:outer)
  (:export ; stack
   #:stack*
   #:stack))
