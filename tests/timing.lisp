;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations-tests)

(defparameter *a* (ao:generate* 'double-float (lambda () (random 1d0))
                                '(100 100 100)))

(defun permute-loop (array permutation &optional (n 10))
  (loop repeat n
        do (ao:permute array permutation)))

(time (permute-loop *a* '(2 1 0)))
