;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; shorthand functions

(defgeneric as-array (object)
  (:documentation "Return the contents of OBJECT as an array.  Exact semantics depends on OBJECT, but generally objects which contain elements in a rectilinear coordinate system should have a natural mapping to arrays.

When the second value is T, the array itself does not share structure with OBJECT, but its elements may.  Otherwise, it is indeterminate whether the two objects share structure, and consequences of modifying the result are not defined.  Methods are encouraged but not required to return a second value.")
  (:method ((array array))
    array))

(defmacro define-array-function (name (array-argument &rest other-arguments) &body body)
  "Define a generic function with two methods:

  1. one specializing the first argument to arrays, calling body

  2. the converting the first argument with as-array and calling the function with that, other arguments unchanged."
  (let+ (((&values body declarations docstring) (parse-body body :documentation t)))
    `(defgeneric ,name (,array-argument ,@other-arguments)
       ,@(when docstring `((:documentation ,docstring)))
       (:method ((,array-argument array) ,@other-arguments)
         ,@declarations
         ,@body)
       (:method (,array-argument ,@other-arguments)
         (,name (as-array ,array-argument) ,@other-arguments)))))

(define-array-function size (array)
  "Return the total size of ARRAY."
  (array-total-size array))

(define-array-function rank (array)
  "Return the rank of ARRAY."
  (array-rank array))

(define-array-function dim (array axis)
  "Return specificed dimension of ARRAY."
  (array-dimension array axis))

(define-array-function dims (array)
  "Return the list of dimensions."
  (array-dimensions array))

(define-array-function nrow (array)
  "Number of rows.  Will signal an error if ARRAY is not a matrix."
  (assert (= (rank array) 2))
  (dim array 0))

(define-array-function ncol (array)
  "Number of columns.  Will signal an error if ARRAY is not a matrix."
  (assert (= (rank array) 2))
  (dim array 1))

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

(define-let+-expansion (&dims dimensions :value-var value-var
                                         :body-var body-var)
  "Dimensions of array-like object."
  `(let+ ((,dimensions (dims ,value-var)))
     ,@body-var))
