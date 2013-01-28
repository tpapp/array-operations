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
    (aops:walk-subscripts ('(2 3) subscripts position)
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
    (assert-equalp #(0 1) (aops:displace a 2))
    (assert-equalp #2A((2 3)) (aops:displace a '(1 2) 2))
    ;; flatten
    (assert-equalp #(0 1 2 3 4 5) (aops:flatten a))
    ;; split
    (assert-equalp a (aops:split a 0))
    (assert-equalp #(#(0 1) #(2 3) #(4 5)) (aops:split a 1))
    (assert-equalp a (aops:split a 2))
    ;; sub
    (assert-equalp #(4 5) (aops:sub a 2))
    (assert-equalp 4 (aops:sub a 2 0))
    (let ((b (copy-array a)))
      (assert-equalp #(7 9) (setf (aops:sub b 1) #(7 9)))
      (assert-equalp #2A((0 1) (7 9) (4 5)) b)
      (assert-condition error (setf (aops:sub 0 2) #(1))))
    ;; partition
    (assert-equalp #2A((2 3) (4 5)) (aops:partition a 1))
    (assert-equalp #2A((2 3)) (aops:partition a 1 2))
    (assert-condition error (aops:partition a 0 9))
    (let ((b (copy-array a)))
      (setf (aops:partition b 1) #2A((11 13) (17 19)))
      (assert-equalp #2A((0 1) (11 13) (17 19)) b))
    ;; combine
    (assert-equalp a (aops:combine (aops:split a 0)))
    (assert-equalp a (aops:combine (aops:split a 1)))
    (assert-equalp a (aops:combine (aops:split a 2)))
    (let ((b #(1 #(2 3) 4))
          (c 9))
      (assert-equalp b (aops:combine b))
      (assert-equalp c (aops:combine c)))
    ;; subvec
    (let ((b (copy-array (aops:flatten a))))
      (assert-equalp #(2 3 4 5) (aops:subvec b 2))
      (assert-equalp #(3 4) (aops:subvec b 3 5))
      (assert-condition error (aops:subvec b 0 9))
      (assert-equalp #(7 9) (setf (aops:subvec b 3 5) #(7 9)))
      (assert-equalp #(0 1 2 7 9 5) b)
      (assert-condition error (setf (aops:subvec b 3 5) #(7))))
    ;; reshape & variances
    (assert-equalp #2A((0 1 2) (3 4 5)) (aops:reshape a '(2 3)))
    (assert-equalp #2A((0 1 2 3 4 5)) (aops:reshape-row a))
    (assert-equalp #2A((0) (1) (2) (3) (4) (5)) (aops:reshape-col a))))

;;; transformations

(deftest generate (tests)
  (let ((a (aops:generate #'identity '(3 2) :position))
        (b (aops:generate #'identity '(2 3) :subscripts)))
    (assert-equalp #2A((0 1)
                       (2 3)
                       (4 5))
      a)
    (assert-equalp #2A(((0 0) (0 1) (0 2))
                       ((1 0) (1 1) (1 2)))
      b)
    (assert-equalp #2A(((0 0 0) (1 0 1)))
      (aops:generate #'cons '(1 2) :position-and-subscripts))))

(defun permute% (subscripts-mapping array)
  "Helper function for testing permutation.  Permutes ARRAY using SUBSCRIPTS-MAPPING, should return the permuted arguments as a list."
  (let+ ((dimensions (array-dimensions array))
         ((&flet map% (subscripts)
            (apply subscripts-mapping subscripts))))
    (aprog1 (make-array (map% dimensions)
                        :element-type (array-element-type array))
      (aops:walk-subscripts-list (dimensions subscripts)
        (setf (apply #'aref it (map% subscripts))
              (apply #'aref array subscripts))))))

(deftest permutations (tests)
  (assert-equalp #*10110 (aops::permutation-flags '(0 3 2) 5))
  (assert-condition error (aops::check-permutation '(0 1 1)))
  (assert-equalp '(0 1 4) (aops:complement-permutation '(3 2) 5))
  (assert-equalp '(3 2 0 1 4) (aops:complete-permutation '(3 2) 5))
  (assert-equalp '(0 1 2 3) (aops:invert-permutation '(0 1 2 3)))
  (assert-equalp '(1 3 2 0) (aops:invert-permutation '(3 0 2 1)))
  (let+ (((&flet assert-equalp-i2 (permutation)
            (assert-equalp permutation
                (aops:invert-permutation (aops:invert-permutation permutation))))))
    (assert-equalp-i2 '(0 1 2 3))
    (assert-equalp-i2 '(3 0 2 1))))

(deftest permute (tests)
  (let ((a (aops:generate #'identity '(3 2) :position)))
    (assert-equalp a (aops:permute '(0 1) a))
    (assert-equalp  #2A((0 2 4)
                        (1 3 5))
      (aops:permute '(1 0) a))
    (assert-condition aops:permutation-repeated-index (aops:permute '(0 0) a))
    (assert-condition aops:permutation-invalid-index (aops:permute '(2 0) a))
    (assert-condition aops:permutation-incompatible-rank (aops:permute '(0) a)))
  (let ((p (alexandria:shuffle (list 0 1 2 3 4)))
        (a (aops:generate (lambda () (random 100)) '(2 3 4 5 6)))
        (*lift-equality-test* #'equalp))
    (assert-equalp p (aops:invert-permutation (aops:invert-permutation p)))
    (assert-equalp a (aops:permute (aops:invert-permutation p) (aops:permute p a))))
  (let ((a (aops:generate #'identity '(2 2 2) :position)))
    (assert-equalp (aops:permute '(2 0 1) a)
        (permute% (lambda (a b c) (list c a b)) a))))

(deftest each (tests)
  (let ((a (aops:generate #'identity '(2 5) :position)))
    (assert-equalp (aops:generate #'1+ '(2 5) :position) (aops:each #'1+ a)))
  (assert-equalp #(1 1 2 3) (aops:each #'- #(2 3 5 7) #(1 2 3 4))))

(deftest margin (tests)
  (let ((a (aops:generate #'identity '(3 5) :position)))
    (assert-equalp #(10 35 60) (aops:margin (curry #'reduce #'+) a 1))
    (assert-equalp #(0 66 168 312 504) (aops:margin (curry #'reduce #'*) a 0))))

(deftest recycle (tests)
  (assert-equalp (make-array '(3 4 2 1) :initial-element 1)
      (aops:recycle 1 :inner '(2 1) :outer '(3 4)))
  (let ((a (aops:generate #'identity '(2 3) :position)))
    (assert-equalp a (aops:recycle a))
    (assert-equalp (aops:generate (lambda (p) (floor p 2)) '(2 3 2) :position)
        (aops:recycle a :inner 2))
    (assert-equalp (aops:generate (lambda (p) (rem p 6)) '(2 2 3 1) :position)
        (aops:recycle a :inner 1 :outer 2))))

(deftest outer (tests)
  (let ((a #(2 3 5))
        (b #(7 11))
        (c #2A((7 11)
               (13 17))))
    (assert-equalp #2A((14 22)
                       (21 33)
                       (35 55))
      (aops:outer #'* a b))
    (assert-equalp #3A(((14 21 35) (22 33 55))
                       ((26 39 65) (34 51 85)))
      (aops:outer #'* c a))
    (assert-equalp (aops:combine (aops:each (lambda (v)
                                          (aops:each (curry #'* v) c))
                                        a))
      (aops:outer #'* a c))))

;;; stack

(deftest stack0 (tests)
  (assert-equalp #(0 1 2 3 4 5 6) (aops:stack 0 #(0 1 2 3) #(4 5 6)))
  (assert-equalp #2A((0 1)
                     (2 3)
                     (5 7))
    (aops:stack 0
              #2A((0 1)
                  (2 3))
              #2A((5 7))))
  (assert-condition error (aops:stack 0 #(0 1) #2A((0 1 2 3))))
  (assert-condition error (aops:stack 0 #2A((1)) #2A((0 1)))))

(deftest stack (tests)
  (assert-equalp #2A((0 1 5)
                     (2 3 9))
    (aops:stack 1
              #2A((0 1)
                  (2 3))
              #2A((5) (9)))))
