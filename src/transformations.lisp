;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; creating arrays

(defun generate* (element-type function dimensions &optional arguments)
  (aprog1 (make-array dimensions :element-type element-type)
    (ecase arguments
      ((nil)
       (dotimes (position (array-total-size it))
         (setf (row-major-aref it position)
               (funcall function))))
      (:position
       (walk-subscripts (dimensions subscripts position)
         (setf (row-major-aref it position) (funcall function position))))
      (:subscripts
       (walk-subscripts-list (dimensions subscripts position)
         (setf (row-major-aref it position)
               (funcall function subscripts))))
      (:position-and-subscripts
       (walk-subscripts-list (dimensions subscripts position)
         (setf (row-major-aref it position)
               (funcall function position subscripts)))))))

(defun generate (function dimensions &optional arguments)
  (generate* t function dimensions arguments))



;;; permutations

(define-condition permutation-repeated-index (error)
  ((index :initarg :index)))

(define-condition permutation-invalid-index (error)
  ((index :initarg :index)))

(define-condition permutation-incompatible-rank (error)
  ())

(defun permutation-flags% (permutation &optional (rank (length permutation)))
  (aprog1 (make-array rank
                      :element-type 'bit :initial-element 0)
    (map nil (lambda (p)
               (assert (and (integerp p) (< -1 p rank)) ()
                       'permutation-invalid-index :index p)
               (assert (zerop (aref it p)) ()
                       'permutation-repeated-index :index p)
               (setf (aref it p) 1))
         permutation)))

(defun check-permutation (permutation
                          &optional (rank (length permutation) rank?))
  "Check if PERMUTATION is a valid permutation (of the given RANK), and signal
an error if necessary."
  (when rank?
    (assert (= rank (length permutation)) ()
            'permutation-incompatible-rank ))
  (assert (every #'plusp (permutation-flags% permutation)) ()
          'permutation-incompatible-rank))

(defun complement-permutation (permutation rank)
  (loop for f across (permutation-flags% permutation rank)
        for index from 0
        when (zerop f)
        collect index))

(defun invert-permutation (permutation)
  "Invert a permutation."
  (check-permutation permutation)
  (aprog1 (make-array (length permutation) :element-type 'fixnum)
    (map nil (let ((index 0))
               (lambda (p)
                 (setf (aref it p) index)
                 (incf index)))
         permutation)))

(defun permute (array permutation)
  "Return ARRAY with the axes permuted by PERMUTATION, which is a sequence of
indexes.  Specifically, an array A is transformed to B, where

  B[b_1,...,b_n] = A[a_1,...,a_n] with b_i=a_{P[i]}

P is the permutation.

Array element type is preserved."
  (let+ ((rank (array-rank array))
         (dimensions (array-dimensions array))
         ((&flet map-subscripts (subscripts-vector)
            (map 'list (curry #'aref subscripts-vector) permutation))))
    (check-permutation permutation rank)
    (aprog1 (make-array (map-subscripts (coerce dimensions 'vector))
                        :element-type (array-element-type array))
      (walk-subscripts (dimensions subscripts position)
        (setf (apply #'aref it (map-subscripts subscripts))
              (row-major-aref array position))))))



;;; margin

(defun each* (element-type function array &rest other-arrays)
  (aprog1 (make-array (array-dimensions array) :element-type element-type)
    (assert (apply #'same-dimensions? array other-arrays))
    (apply #'map-into (flatten it) function
           (flatten array) (mapcar #'flatten other-arrays))))

(defun each (function array &rest other-arrays)
  (apply #'each* t function array other-arrays))

(defun margin* (element-type function array inner
                &optional (outer (complement-permutation inner (array-rank array))))
  (each* element-type function
         (split (permute array (append outer inner)) (length outer))))

(defun margin (function array inner
               &optional (outer (complement-permutation inner (array-rank array))))
  (margin* t function array inner outer))
