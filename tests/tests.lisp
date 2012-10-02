;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:array-operations-tests
  (:use #:cl #:alexandria #:anaphora #:let-plus #:lift)
  (:export #:run))

(in-package #:array-operations-tests)

(deftestsuite array-operations-tests () ()
  (:equality-test #'equalp))

(defun run ()
  "Run all the tests for LLA."
  (run-tests :suite 'array-operations-tests))



;;; utilities

(addtest walk-subscripts
  (let (result)
    (ao:walk-subscripts ('(2 3) subscripts position)
      (push (cons position (copy-seq subscripts)) result))
    (ensure-same (reverse result)
                 '((0 . #(0 0))
                   (1 . #(0 1))
                   (2 . #(0 2))
                   (3 . #(1 0))
                   (4 . #(1 1))
                   (5 . #(1 2))))))

(addtest displacement
  (let ((a #2A((0 1) (2 3) (4 5))))
    ;; displace
    (ensure-same (ao:displace a 2) #(0 1))
    (ensure-same (ao:displace a '(1 2) 2) #2A((2 3)))
    ;; flatten
    (ensure-same (ao:flatten a) #(0 1 2 3 4 5))
    ;; split
    (ensure-same (ao:split a 0) a)
    (ensure-same (ao:split a 1) #(#(0 1) #(2 3) #(4 5)))
    (ensure-same (ao:split a 2) a)
    ;; sub
    (ensure-same (ao:sub a 2) #(4 5))
    (ensure-same (ao:sub a 2 0) 4)
    (let ((b (copy-array a)))
      (ensure-same (setf (ao:sub b 1) #(7 9)) #(7 9))
      (ensure-same b #2A((0 1) (7 9) (4 5)))
      (ensure-error (setf (ao:sub 0 2) #(1))))
    ;; partition
    (ensure-same (ao:partition a 1) #2A((2 3) (4 5)))
    (ensure-same (ao:partition a 1 2) #2A((2 3)))
    (ensure-error (ao:partition a 0 9))
    ;; combine
    (ensure-same (ao:combine (ao:split a 0)) a)
    (ensure-same (ao:combine (ao:split a 1)) a)
    (ensure-same (ao:combine (ao:split a 2)) a)
    (let ((b #(1 #(2 3) 4))
          (c 9))
      (ensure-same (ao:combine b) b)
      (ensure-same (ao:combine c) c))
    ;; subvec
    (let ((b (copy-array (ao:flatten a))))
      (ensure-same (ao:subvec b 2) #(2 3 4 5))
      (ensure-same (ao:subvec b 3 5) #(3 4))
      (ensure-error (ao:subvec b 0 9))
      (ensure-same (setf (ao:subvec b 3 5) #(7 9)) #(7 9))
      (ensure-same b #(0 1 2 7 9 5))
      (ensure-error (setf (ao:subvec b 3 5) #(7))))
    ;; reshape
    (ensure-same (ao:reshape a '(2 3)) #2A((0 1 2) (3 4 5)))))

(addtest generate
  (let ((a (ao:generate #'identity '(3 2) :position))
        (b (ao:generate #'identity '(2 3) :subscripts)))
    (ensure-same a #2A((0 1)
                       (2 3)
                       (4 5)))
    (ensure-same b #2A(((0 0) (0 1) (0 2))
                       ((1 0) (1 1) (1 2))))
    (ensure-same (ao:generate #'cons '(1 2) :position-and-subscripts)
                 #2A(((0 0 0) (1 0 1))))))

(defun permute% (array subscripts-mapping)
  "Helper function for testing permutation.  Permutes ARRAY using
SUBSCRIPTS-MAPPING, should return the permuted arguments as a list."
  (let+ ((dimensions (array-dimensions array))
         ((&flet map% (subscripts)
            (apply subscripts-mapping subscripts))))
    (aprog1 (make-array (map% dimensions)
                        :element-type (array-element-type array))
      (ao:walk-subscripts-list (dimensions subscripts)
        (setf (apply #'aref it (map% subscripts))
              (apply #'aref array subscripts))))))

(addtest permute
  (let ((a (ao:generate #'identity '(3 2) :position)))
    (ensure-same (ao:permute a '(0 1)) a)
    (ensure-same (ao:permute a '(1 0)) #2A((0 2 4)
                                           (1 3 5)))
    (ensure-condition ao:permutation-repeated-index (ao:permute a '(0 0)))
    (ensure-condition ao:permutation-invalid-index (ao:permute a '(2 0)))
    (ensure-condition ao:permutation-incompatible-rank (ao:permute a '(0))))
  (let ((a (ao:generate #'identity '(2 2 2) :position)))
    (ensure-same (ao:permute a '(2 0 1))
                 (permute% a (lambda (a b c) (list c a b))))))
