;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

;;; utilities used internally, not exported

(defun product (dimensions)
  "Product of elements in the argument.  NOT EXPORTED."
  (reduce #'* dimensions))

(define-modify-macro multf (&rest values) * "Multiply by the arguments")

(defun same-dimensions? (array &rest arrays)
  "Test if arguments have the same dimensions.  NOT EXPORTED."
  (let ((dimensions (array-dimensions array)))
    (every (lambda (array)
             (equal dimensions (array-dimensions array)))
           arrays)))

(defun ensure-dimensions (object)
  "Return a list of dimensions corresponding to OBJECT.  Positive integers are
treated as dimensions of rank 1, lists are returned as they are, and arrays
are queried for their dimensions.

OBJECTS accepted by this function as valid dimensions are called `dimension
specifications' in this library."
  (aetypecase object
    ((integer 0) (list it))
    (list it)
    (array (array-dimensions it))))

(defmacro walk-subscripts ((dimensions subscripts
                            &optional (position (gensym "POSITION")))
                           &body body)
  "Iterate over the subscripts of an array with given DIMENSIONS.  SUBSCRIPTS
contains the current subscripts as a vector of fixnums, POSITION has the
row-major index.  Consequences are undefined if either POSITION or SUBSCRIPTS
is modified."
  (check-type position symbol)
  (check-type subscripts symbol)
  (with-unique-names (rank last increment dimensions-var)
    `(let+ ((,dimensions-var (ensure-dimensions ,dimensions))
            (,rank (length ,dimensions-var))
            (,dimensions-var (make-array ,rank
                                         :element-type 'fixnum
                                         :initial-contents ,dimensions-var))
            (,last (1- ,rank))
            (,subscripts (make-array ,rank
                                     :element-type 'fixnum
                                     :initial-element 0))
            ((&labels ,increment (index)
               (unless (minusp index)
                 (when (= (incf (aref ,subscripts index))
                          (aref ,dimensions-var index))
                   (setf (aref ,subscripts index) 0)
                   (,increment (1- index)))))))
       (dotimes (,position (product ,dimensions-var))
         ,@body
         (,increment ,last)))))

(defmacro walk-subscripts-list ((dimensions subscripts
                                 &optional (position (gensym "POSITION")))
                                &body body)
  "Like WALK-SUBSCRIPTS, but SUBSCRIPTS is a newly created list for each
position that does not share structure and can be freely used/modified/kept
etc."
  (with-unique-names (subscripts-vector)
    `(walk-subscripts (,dimensions ,subscripts-vector ,position)
       (let ((,subscripts (coerce ,subscripts-vector 'list)))
         ,@body))))
