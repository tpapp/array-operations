;;;; package.lisp

(defpackage #:array-operations
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus
        #:optima)
  (:nicknames #:aops)
  (:shadow #:flatten)
  (:export ; utilities
   #:walk-subscripts
   #:walk-subscripts-list)
  (:export ; general
   #:as-array
   #:element-type
   #:dims
   #:size
   #:rank
   #:dim
   #:&dims
   #:nrow
   #:ncol
   #:array-matrix
   #:matrix?
   #:square-matrix?
   #:make-array-like)
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
   #:each*
   #:each
   #:margin*
   #:margin
   #:recycle
   #:outer*
   #:outer)
  (:export ; stack
   #:copy-row-major-block
   #:stack-rows-copy
   #:stack-rows*
   #:stack-rows
   #:stack-cols-copy
   #:stack-cols*
   #:stack-cols
   #:stack*
   #:stack))
