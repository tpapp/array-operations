;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:array-operations-tests
  (:use #:cl #:alexandria #:anaphora #:clunit #:let-plus)
  (:export #:run))

(cl:in-package #:array-operations-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run all the tests for LLA."
  (run-suite 'tests :use-debugger interactive?))



;;; utilities

(deftest walk-subscripts (tests)
  (let (result)
    (ao:walk-subscripts ('(2 3) subscripts position)
      (push (cons position (copy-seq subscripts)) result))
    (assert-equalp '((0 . #(0 0))
                     (1 . #(0 1))
                     (2 . #(0 2))
                     (3 . #(1 0))
                     (4 . #(1 1))
                     (5 . #(1 2)))
        (reverse result))))



;;; displacement

(deftest displacement (tests)
  (let ((a #2A((0 1) (2 3) (4 5))))
    ;; displace
    (assert-equalp #(0 1) (ao:displace a 2))
    (assert-equalp #2A((2 3)) (ao:displace a '(1 2) 2))
    ;; flatten
    (assert-equalp #(0 1 2 3 4 5) (ao:flatten a))
    ;; split
    (assert-equalp a (ao:split a 0))
    (assert-equalp #(#(0 1) #(2 3) #(4 5)) (ao:split a 1))
    (assert-equalp a (ao:split a 2))
    ;; sub
    (assert-equalp #(4 5) (ao:sub a 2))
    (assert-equalp 4 (ao:sub a 2 0))
    (let ((b (copy-array a)))
      (assert-equalp #(7 9) (setf (ao:sub b 1) #(7 9)))
      (assert-equalp #2A((0 1) (7 9) (4 5)) b)
      (assert-condition error (setf (ao:sub 0 2) #(1))))
    ;; partition
    (assert-equalp #2A((2 3) (4 5)) (ao:partition a 1))
    (assert-equalp #2A((2 3)) (ao:partition a 1 2))
    (assert-condition error (ao:partition a 0 9))
    (let ((b (copy-array a)))
      (setf (ao:partition b 1) #2A((11 13) (17 19)))
      (assert-equalp #2A((0 1) (11 13) (17 19)) b))
    ;; combine
    (assert-equalp a (ao:combine (ao:split a 0)))
    (assert-equalp a (ao:combine (ao:split a 1)))
    (assert-equalp a (ao:combine (ao:split a 2)))
    (let ((b #(1 #(2 3) 4))
          (c 9))
      (assert-equalp b (ao:combine b))
      (assert-equalp c (ao:combine c)))
    ;; subvec
    (let ((b (copy-array (ao:flatten a))))
      (assert-equalp #(2 3 4 5) (ao:subvec b 2))
      (assert-equalp #(3 4) (ao:subvec b 3 5))
      (assert-condition error (ao:subvec b 0 9))
      (assert-equalp #(7 9) (setf (ao:subvec b 3 5) #(7 9)))
      (assert-equalp #(0 1 2 7 9 5) b)
      (assert-condition error (setf (ao:subvec b 3 5) #(7))))
    ;; reshape & variances
    (assert-equalp #2A((0 1 2) (3 4 5)) (ao:reshape a '(2 3)))
    (assert-equalp #2A((0 1 2 3 4 5)) (ao:reshape-row a))
    (assert-equalp #2A((0) (1) (2) (3) (4) (5)) (ao:reshape-col a))))



;;; transformations

(deftest generate (tests)
  (let ((a (ao:generate #'identity '(3 2) :position))
        (b (ao:generate #'identity '(2 3) :subscripts)))
    (assert-equalp #2A((0 1)
                       (2 3)
                       (4 5))
      a)
    (assert-equalp #2A(((0 0) (0 1) (0 2))
                       ((1 0) (1 1) (1 2)))
      b)
    (assert-equalp #2A(((0 0 0) (1 0 1)))
      (ao:generate #'cons '(1 2) :position-and-subscripts))))

(defun permute% (subscripts-mapping array)
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

(deftest permutations (tests)
  (assert-equalp #*10110 (ao::permutation-flags '(0 3 2) 5))
  (assert-condition error (ao::check-permutation '(0 1 1)))
  (assert-equalp '(0 1 4) (ao:complement-permutation '(3 2) 5))
  (assert-equalp '(3 2 0 1 4) (ao:complete-permutation '(3 2) 5))
  (assert-equalp '(0 1 2 3) (ao:invert-permutation '(0 1 2 3)))
  (assert-equalp '(1 3 2 0) (ao:invert-permutation '(3 0 2 1)))
  (let+ (((&flet assert-equalp-i2 (permutation)
            (assert-equalp permutation
                (ao:invert-permutation (ao:invert-permutation permutation))))))
    (assert-equalp-i2 '(0 1 2 3))
    (assert-equalp-i2 '(3 0 2 1))))

(deftest permute (tests)
  (let ((a (ao:generate #'identity '(3 2) :position)))
    (assert-equalp a (ao:permute '(0 1) a))
    (assert-equalp  #2A((0 2 4)
                        (1 3 5))
      (ao:permute '(1 0) a))
    (assert-condition ao:permutation-repeated-index (ao:permute '(0 0) a))
    (assert-condition ao:permutation-invalid-index (ao:permute '(2 0) a))
    (assert-condition ao:permutation-incompatible-rank (ao:permute '(0) a)))
  (let ((p (alexandria:shuffle (list 0 1 2 3 4)))
        (a (ao:generate (lambda () (random 100)) '(2 3 4 5 6)))
        (*lift-equality-test* #'equalp))
    (assert-equalp p (ao:invert-permutation (ao:invert-permutation p)))
    (assert-equalp a (ao:permute (ao:invert-permutation p) (ao:permute p a))))
  (let ((a (ao:generate #'identity '(2 2 2) :position)))
    (assert-equalp (ao:permute '(2 0 1) a)
        (permute% (lambda (a b c) (list c a b)) a))))

(deftest each (tests)
  (let ((a (ao:generate #'identity '(2 5) :position)))
    (assert-equalp (ao:generate #'1+ '(2 5) :position) (ao:each #'1+ a)))
  (assert-equalp #(1 1 2 3) (ao:each #'- #(2 3 5 7) #(1 2 3 4))))

(deftest margin (tests)
  (let ((a (ao:generate #'identity '(3 5) :position)))
    (assert-equalp #(10 35 60) (ao:margin (curry #'reduce #'+) a 1))
    (assert-equalp #(0 66 168 312 504) (ao:margin (curry #'reduce #'*) a 0))))

(deftest recycle (tests)
  (assert-equalp (make-array '(3 4 2 1) :initial-element 1)
      (ao:recycle 1 :inner '(2 1) :outer '(3 4)))
  (let ((a (ao:generate #'identity '(2 3) :position)))
    (assert-equalp a (ao:recycle a))
    (assert-equalp (ao:generate (lambda (p) (floor p 2)) '(2 3 2) :position)
        (ao:recycle a :inner 2))
    (assert-equalp (ao:generate (lambda (p) (rem p 6)) '(2 2 3 1) :position)
        (ao:recycle a :inner 1 :outer 2))))



;;; stack

(deftest stack0 (tests)
  (assert-equalp #(0 1 2 3 4 5 6) (ao:stack 0 #(0 1 2 3) #(4 5 6)))
  (assert-equalp #2A((0 1)
                     (2 3)
                     (5 7))
    (ao:stack 0
              #2A((0 1)
                  (2 3))
              #2A((5 7))))
  (assert-condition error (ao:stack 0 #(0 1) #2A((0 1 2 3))))
  (assert-condition error (ao:stack 0 #2A((1)) #2A((0 1)))))

(deftest stack (tests)
  (assert-equalp #2A((0 1 5)
                     (2 3 9))
    (ao:stack 1
              #2A((0 1)
                  (2 3))
              #2A((5) (9)))))
