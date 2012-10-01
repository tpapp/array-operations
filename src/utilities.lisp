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

(defmacro walk-subscripts ((dimensions subscripts
                            &optional (position (gensym "POSITION")))
                           &body body)
  "Iterate over the subscripts of an array with given DIMENSIONS.  SUBSCRIPTS
contains the current subscripts as a vector of fixnums, POSITION has the
row-major index.  Consequences are undefined if either POSITION or SUBSCRIPTS
is modified."
  (check-type position symbol)
  (check-type subscripts symbol)
  (with-unique-names (rank last increment)
    (once-only (dimensions)
      `(let+ ((,rank (length ,dimensions))
              (,dimensions (make-array ,rank
                                       :element-type 'fixnum
                                       :initial-contents ,dimensions))
              (,last (1- ,rank))
              (,subscripts (make-array ,rank
                                       :element-type 'fixnum
                                       :initial-element 0))
              ((&labels ,increment (index)
                 (unless (minusp index)
                   (when (= (incf (aref ,subscripts index))
                            (aref ,dimensions index))
                     (setf (aref ,subscripts index) 0)
                     (,increment (1- index)))))))
         (dotimes (,position (product ,dimensions))
           ,@body
           (,increment ,last))))))
