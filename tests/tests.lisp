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



;;; displacement

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
    (let ((b (copy-array a)))
      (setf (ao:partition b 1) #2A((11 13) (17 19)))
      (ensure-same b #2A((0 1) (11 13) (17 19))))
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
    ;; reshape & variances
    (ensure-same (ao:reshape a '(2 3)) #2A((0 1 2) (3 4 5)))
    (ensure-same (ao:reshape-row a) #2A((0 1 2 3 4 5)))
    (ensure-same (ao:reshape-col a) #2A((0) (1) (2) (3) (4) (5)))))



;;; transformations

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

(addtest permutations
  (ensure-same (ao::permutation-flags '(0 3 2) 5) #*10110)
  (ensure-error (ao::check-permutation '(0 1 1)))
  (ensure-same (ao:complement-permutation '(3 2) 5) '(0 1 4))
  (ensure-same (ao:complete-permutation '(3 2) 5) '(3 2 0 1 4))
  (ensure-same (ao:invert-permutation '(0 1 2 3)) '(0 1 2 3))
  (ensure-same (ao:invert-permutation '(3 0 2 1)) '(1 3 2 0))
  (let+ (((&flet invert-twice (permutation)
            (ao:invert-permutation (ao:invert-permutation permutation))))
         ((&macrolet ensure-same-i2 (permutation)
            (once-only (permutation)
              `(ensure-same ,permutation (invert-twice ,permutation))))))
    (ensure-same-i2 '(0 1 2 3))
    (ensure-same-i2 '(3 0 2 1))))

(addtest permute
  (let ((a (ao:generate #'identity '(3 2) :position)))
    (ensure-same (ao:permute '(0 1) a) a)
    (ensure-same (ao:permute '(1 0) a) #2A((0 2 4)
                                           (1 3 5)))
    (ensure-condition ao:permutation-repeated-index (ao:permute '(0 0) a))
    (ensure-condition ao:permutation-invalid-index (ao:permute '(2 0) a))
    (ensure-condition ao:permutation-incompatible-rank (ao:permute '(0) a)))
  (let ((p (alexandria:shuffle (list 0 1 2 3 4)))
        (a (ao:generate (lambda () (random 100)) '(2 3 4 5 6)))
        (*lift-equality-test* #'equalp))
    (ensure-same (ao:invert-permutation (ao:invert-permutation p)) p)
    (ensure-same (ao:permute (ao:invert-permutation p) (ao:permute p a)) a))
  (let ((a (ao:generate #'identity '(2 2 2) :position)))
    (ensure-same (ao:permute '(2 0 1) a)
                 (permute% (lambda (a b c) (list c a b)) a))))

(addtest each
  (let ((a (ao:generate #'identity '(2 5) :position)))
    (ensure-same (ao:each #'1+ a) (ao:generate #'1+ '(2 5) :position)))
  (ensure-same (ao:each #'- #(2 3 5 7) #(1 2 3 4)) #(1 1 2 3)))

(addtest margin
  (let ((a (ao:generate #'identity '(3 5) :position)))
    (ensure-same (ao:margin (curry #'reduce #'+) a 1) #(10 35 60))
    (ensure-same (ao:margin (curry #'reduce #'*) a 0) #(0 66 168 312 504))))

(addtest recycle
  (ensure-same (ao:recycle 1 :inner '(2 1) :outer '(3 4))
               (make-array '(3 4 2 1) :initial-element 1))
  (let ((a (ao:generate #'identity '(2 3) :position)))
    (ensure-same (ao:recycle a) a)
    (ensure-same (ao:recycle a :inner 2)
                 (ao:generate (lambda (p) (floor p 2)) '(2 3 2) :position))
    (ensure-same (ao:recycle a :inner 1 :outer 2)
                 (ao:generate (lambda (p) (rem p 6)) '(2 2 3 1) :position))))



;;; stack

(addtest stack0
  (ensure-same (ao:stack 0 #(0 1 2 3) #(4 5 6)) #(0 1 2 3 4 5 6))
  (ensure-same (ao:stack 0 #2A((0 1)
                               (2 3))
                         #2A((5 7)))
               #2A((0 1)
                   (2 3)
                   (5 7)))
  (ensure-error (ao:stack 0 #(0 1) #2A((0 1 2 3))))
  (ensure-error (ao:stack 0 #2A((1)) #2A((0 1)))))

(addtest stack
  (ensure-same (ao:stack 1 #2A((0 1)
                               (2 3))
                         #2A((5) (9)))
               #2A((0 1 5)
                   (2 3 9))))
