![Project Status: Abandoned – Initial development has started, but there
has not yet been a stable, usable release; the project has been
abandoned and the author(s) do not intend on continuing
development.](http://www.repostatus.org/badges/latest/abandoned.svg)

This library is [**abandonned**](https://tpapp.github.io/post/orphaned-lisp-libraries/) by the original author. A fork is maintained at https://github.com/Lisp-Stat/array-operations.

## NOTE 2019-05-16

@bendudson and others have continued maintaining a fork of this library at <https://github.com/bendudson/array-operations>.

!!! important
    This is an **alpha** release. All the code works and unit tests are expected to run perfectly, but the operations are not optimized and the API change.

## Introduction

`array-operations` is a Common Lisp library that facilitates working
with Common Lisp arrays using syntax and semantics that work well with
the rest of the language.

*The library previously available under this name is deprecated, but you
can find it [here](https://github.com/tpapp/array-operations-old).*

## A quick tour of the library

### Shorthand for frequently used Common Lisp array functions
---------------------------------------------------------

The library defines the following short function names that are synomyms
for Common Lisp operations:

 | array-operations   | Common Lisp                     |
 | ------------------ | ------------------------------- |
 | size               | array-total-size                |
 | rank               | array-rank                      |
 | dim                | array-dimension                 |
 | dims               | array-dimensions                |
 | nrow               | *number of rows in matrix*      |
 | ncol               | *number of columns in matrix*   |

The `array-operations` package has the nickname `ao`, so you can use,
for example, `(ao:size my-array)` without `use`'ing the package.

### Displaced arrays for fun and profit

> displaced array n. an array which has no storage of its own, but which
> is instead indirected to the storage of another array, called its
> target, at a specified offset, in such a way that any attempt to
> access the displaced array implicitly references the target array.
> (CLHS Glossary)

Displaced arrays are one of the niftiest features of Common Lisp. When
an array is displaced to another array, it shares structure with (part
of) that array. The two arrays do not need to have the same dimensions,
in fact, the dimensions do not be related at all as long as the
displaced array fits inside the original one. The row-major index of the
former in the latter is called the *offset* of the displacement.

Displaced arrays are usually constructed using `make-array`, but this
library also provides `displace` for that purpose:

```lisp
(defparameter *a* #2A((1 2 3) (4 5 6)))
(ao:displace *a* 2 1) ; => #(2 3)
```

`flatten` displaces to a row-major array:

```lisp
(ao:flatten *a*) ; => #(1 2 3 4 5 6)
```

The real fun starts with `split`, which splits off subarrays nested
within a given axis:

```lisp
(ao:split *a* 1) ; => #(#(1 2 3) #(4 5 6))
(defparameter *b* #3A(((0 1) (2 3))
                      ((4 5) (6 7))))
(ao:split *b* 0) ; => #3A(((0 1) (2 3)) ((4 5) (6 7)))
(ao:split *b* 1) ; => #(#2A((0 1) (2 3)) #2A((4 5) (6 7)))
(ao:split *b* 2) ; => #2A((#(0 1) #(2 3)) (#(4 5) #(6 7)))
(ao:split *b* 3) ; => #3A(((0 1) (2 3)) ((4 5) (6 7)))
```

Note how splitting at `0` and the rank of the array returns the array
itself.

Now consider `sub`, which returns a specific array, composed of the
elements that would start with given subscripts:

```lisp
(ao:sub *b* 0) ; => #2A((0 1) (2 3))
(ao:sub *b* 0 1) ; => #(2 3)
(ao:sub *b* 0 1 0) ; => 2
```

There is also a `(setf sub)` function.

`partition` returns a consecutive chunk of an array separated along its
first subscript:

```lisp
(ao:partition #2A((0 1)
                  (2 3)
                  (4 5)
                  (6 7)
                  (8 9))
              1 3) ; => #2A((2 3) (4 5))
```

and also has a `(setf partition)` pair.

`combine` is the opposite of `split`:

```lisp
(ao:combine #(#(0 1) #(2 3))) ; => #2A((0 1) (2 3))
```

`subvec` returns a displaced subvector:

```lisp
(ao:subvec #(0 1 2 3 4) 2 4) ; => #(2 3)
```

There is also a `(setf subvec)` function, which is like `(setf subseq)`
except for demanding matching lengths.

Finally, `reshape` can be used to displace arrays into a different
shape:

```lisp
(ao:reshape *a* '(3 2)) ; => #2A((1 2) (3 4) (5 6))
```

You can use `t` for one of the dimensions, to be filled in
automatically:

```lisp
(ao:reshape *b* '(1 t)) ; => #2A((0 1 2 3 4 5 6 7))
```

`reshape-col` and `reshape-row` reshape your array into a column or row
matrix, respectively.

Dimension specifications
------------------------

Functions in the library accept the following in place of dimensions:

-   a list of dimensions (as for `make-array`),
-   a positive integer, which is used as a single-element list,
-   another array, the dimensions of which are used.

The last one allows you to specify dimensions with other arrays. For
example, to reshape an array `a1` to look like `a2`, you can use

```lisp
(ao:reshape a1 a2)
```

instead of the longer form

```lisp
(ao:reshape a1 (ao:dims a2))
```

Array creation and transformations
----------------------------------

When the resulting element type cannot be inferred, functions that
create and transform arrays are provided in pairs: one of these will
allow you to specify the array-element-type of the result, while the
other assumes it is `t`. The former ends with a `*`, and the
`element-type` is always its first argument. I give examples for the
versions without `*`, use the other when you are optimizing your code
and you are sure you can constrain to a given element-type.

**Element traversal order of these functions is unspecified**. The
reason for this is that the library may use parallel code in the future,
so it is unsafe to rely on a particular element traversal order.

`generate` (and `generate*`) allow you to generate arrays using
functions.

```lisp
(ao:generate (lambda () (random 10)) 3) ; => #(6 9 5)
(ao:generate #'identity '(2 3) :position) ; => #2A((0 1 2) (3 4 5))
(ao:generate #'identity '(2 2) :subscripts)
;; => #2A(((0 0) (0 1)) ((1 0) (1 1)))
(ao:generate #'cons '(2 2) :position-and-subscripts)
;; => #2A(((0 0 0) (1 0 1)) ((2 1 0) (3 1 1)))
```

Depending on the last argument, the function will be called with the
(row-major) position, the subscripts, both, or no argument.

`permute` can permutate subscripts (you can also invert, complement, and
complete permutations, look at the docstring and the unit tests).
Transposing is a special case of permute:

```lisp
(ao:permute '(0 1) *a*) ; => #2A((1 2 3) (4 5 6))
```

`each` applies a function to its (array) arguments elementwise:

```lisp
(ao:each #'+ #(0 1 2) #(2 3 5)) ; => #(2 4 7)
```

The semantics of `margin` are more difficult to explain, so perhaps an
example will be more useful. Suppose that you want to calculate column
sums in a matrix. You could `permute` (transpose) the matrix, `split`
its subarrays at rank one (so you get a vector for each row), and apply
the function that calculates the sum. `margin` automates that for you:

```lisp
(ao:margin (lambda (column)
             (reduce #'+ column))
           #2A((0 1)
               (2 3)
               (5 7)) 0) ; => #(7 11)
```

But the function is much more general than this: the arguments `inner`
and `outer` allow arbitrary permutations before splitting.

Finally, `recycle` allows you to recycle arrays along inner and outer
dimensions:

```lisp
(ao:recycle #(2 3) :inner 2 :outer 4)
; => #3A(((2 2) (3 3)) ((2 2) (3 3)) ((2 2) (3 3)) ((2 2) (3 3)))
```

Scalars as 0-dimensional arrays
-------------------------------

Library functions treat non-array objects as if they were equivalent to
0-dimensional arrays: for example, `(ao:split array (rank array))`
returns an array that effectively equivalent (`eq`) to array. Another
example is `recycle`:

```lisp
(ao:recycle 4 :inner '(2 2)) ; => #2A((4 4) (4 4))
```

Stacking
--------

You can also stack compatible arrays along any axis:

```lisp
(defparameter *a1* #(0 1 2))
(defparameter *a2* #(3 5 7))
(ao:stack 0 *a1* *a2*) ; => #(0 1 2 3 5 7)
(ao:stack 1
          (ao:reshape-col *a1*)
          (ao:reshape-col *a2*)) ; => #2A((0 3) (1 5) (2 7))

```

Shared structure
----------------

**Rules for that aren't finalized yet, see the source.** Suggestions are
welcome.

To-do list
==========

benchmark and optimize walk-subscripts and walk-subscripts-list
---------------------------------------------------------------

-   instead of allocating a new list each time, could map into a
    preallocated one
