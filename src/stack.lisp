;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:array-operations)

(defun stack*0 (element-type arrays)
  (let+ ((array-first (car arrays))
         (dim-rest (cdr (array-dimensions array-first)))
         (sum-first
          (reduce #'+ arrays
                  :key (lambda (array)
                         (let+ ((dimensions (array-dimensions array)))
                           (unless (eq array array-first)
                             (assert (equal dim-rest (cdr dimensions)) ()
                                     "Array ~A has incomplatible dimensions"
                                     array))
                           (first dimensions))))))
    (aprog1 (make-array (cons sum-first dim-rest) :element-type element-type)
      (loop with cumulative-sum = 0
            for array in arrays
            do (let* ((dim-first (array-dimension array 0))
                      (end (+ cumulative-sum dim-first)))
                 (setf (partition it cumulative-sum end) array
                       cumulative-sum end))))))

(defun stack* (element-type axis array &rest arrays)
  "Stack array arguments along AXIS.  ELEMENT-TYPE determined the element-type
of the result."
  (declare (optimize debug))
  (if arrays
      (let ((all-arrays (cons array arrays)))
        (if (= axis 0)
            (stack*0 element-type all-arrays)
            (let ((permutation (complete-permutation axis (array-rank array))))
              ;; serious contender for the Least Efficient Implementation Award
              (permute (invert-permutation permutation)
                       (stack*0 element-type
                                (mapcar (curry #'permute permutation)
                                        all-arrays))))))
      array))

(defun stack (axis array &rest arrays)
  "Like STACK*, with element-type T."
  (apply #'stack* t axis array arrays))
