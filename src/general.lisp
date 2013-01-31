;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; shorthand functions

(defgeneric as-array (object)
  (:documentation "Return the contents of OBJECT as an array.  Exact semantics depends on OBJECT, but generally objects which contain elements in a rectilinear coordinate system should have a natural mapping to arrays.

When the second value is T, the array itself does not share structure with OBJECT, but its elements may.  Otherwise, it is indeterminate whether the two objects share structure, and consequences of modifying the result are not defined.  Methods are encouraged but not required to return a second value.")
  (:method ((array array))
    array))

(defgeneric dims (array)
  (:documentation "Return a list of dimensions.

For non-array objects, SIZE, DIM, NROW and NCOL use this method by default, so it is enough to define it (unless efficiency is a concern).")
  (:method ((array array))
    (array-dimensions array)))

(defgeneric size (array)
  (:documentation "Return the total number of elements in array.")
  (:method ((array array))
    (array-total-size array))
  (:method (array)
    (reduce #'* (dims array))))

(defgeneric rank (array)
  (:documentation "Return the rank of ARRAY.")
  (:method ((array array))
    (array-rank array))
  (:method (array)
    (length (dims array))))

(defgeneric dim (array axis)
  (:documentation "Return specificed dimension of ARRAY.")
  (:method ((array array) axis)
    (array-dimension array axis))
  (:method (array axis)
    ;; NOTE: ELT is preferred to NTH here because it signals an error for invalid axes
    (elt (dims array) axis)))

(defgeneric nrow (array)
  (:documentation "Number of rows.  Will signal an error if ARRAY is not a matrix.")
  (:method ((array array))
    (assert (= (rank array) 2))
    (array-dimension array 0))
  (:method (array)
    (let+ (((nrow &ign) (dims array)))
      nrow)))

(defgeneric ncol (array)
  (:documentation "Number of columns.  Will signal an error if ARRAY is not a matrix.")
  (:method ((array array))
    (assert (= (rank array) 2))
    (array-dimension array 1))
  (:method (array)
    (let+ (((&ign ncol) (dims array)))
      ncol)))

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
