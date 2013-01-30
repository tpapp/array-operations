;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; shorthand functions

(declaim (inline size rank dim dims nrow ncol))

(defun size (array)
  "Return the total size of ARRAY."
  (array-total-size array))

(defun rank (array)
  "Return the rank of ARRAY."
  (array-rank array))

(defun dim (array axis)
  "Return specificed dimension of ARRAY."
  (array-dimension array axis))

(defun dims (array)
  "Return the list of dimensions."
  (array-dimensions array))

(defun nrow (array)
  "Number of rows.  Will signal an error if ARRAY is not a matrix."
  (assert (= (rank array) 2))
  (dim array 0))

(defun ncol (array)
  "Number of columns.  Will signal an error if ARRAY is not a matrix."
  (assert (= (rank array) 2))
  (dim array 1))
