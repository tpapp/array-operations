;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; shorthand functions

(deftype matrix (&optional element-type nrow ncol)
  "Matrix type (shorthand for a rank 2 array)."
  `(array ,element-type (,nrow ,ncol)))

(declaim (inline square-matrix?))
(defun square-matrix? (matrix)
  (and (= (array-rank matrix) 2)
       (= (array-dimension matrix 0) (array-dimension matrix 1))))

(deftype square-matrix (&optional element-type dimension)
  "Square matrix type (rank 2 array with equal dimensions."
  `(and (matrix ,element-type ,dimension ,dimension)
        (satisfies square-matrix?)))

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

(defgeneric as-array (object)
  (:documentation "Return the contents of OBJECT as an array.  Exact semantics depends on OBJECT, but generally objects which contain elements in a rectilinear coordinate system should have a natural mapping to arrays.

When the second value is T, the array itself does not share structure with OBJECT, but its elements may.  Otherwise, it is indeterminate whether the two objects share structure, and consequences of modifying the result are not defined.  Methods are encouraged but not required to return a second value."))
