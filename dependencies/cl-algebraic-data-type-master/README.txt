                        CL-ALGEBRAIC-DATA-TYPE
                        ======================
                        
                             Robert Smith

CL-ALGEBRAIC-DATA-TYPE, or ADT, is a library for defining algebraic
data types in a similar spirit to Haskell or Standard ML, as well as
for operating on them.

We can define ADTs using DEFDATA:

(adt:defdata maybe
  (just t)
  nothing)

which will define a new type MAYBE, with a unary constructor JUST, and
a nullary constructor (a simple symbol) NOTHING. The T represents the
data type of that field.

> (just 5)
(JUST 5)
> nothing
NOTHING

We can define our own version of a list via

(adt:defdata liszt
  (kons t liszt)
  knil)

which defines the binary constructor KONS and the nullary constructor
KNIL.

> (kons 1 (kons 2 knil))
(KONS 1 (KONS 2 KNIL))

At the end we will define KAR and KDR.

For efficiency, we might specify the types more exactly. For a POINT
type that supports rectangular and polar coordinates, which is also
mutable, we might have:

(adt:defdata (point :mutable t)
  (rectangular float float)
  (polar float float))

The :MUTABLE keyword signifies that the data is mutable.

When we have constructed a value, we can extract data out of it using MATCH:

> (let ((pt (rectangular 1.0 2.0)))
    (adt:match point pt
      ((rectangular x y) (+ x y))
      ((polar _ _) nil)))

=> 3.0
      
If we did not include the POLAR case, we would get a warning.

> (let ((pt (rectangular 1.0 2.0)))
    (adt:match point pt
      ((rectangular x y) (+ x y))))
; caught WARNING:
;   Non-exhaustive match. Missing cases: (POLAR)
=> 3.0

We can also specify a fall-through:

> (let ((pt (rectangular 1.0 2.0)))
    (adt:match point pt
      ((rectangular x y) (+ x y))
      (_ nil)))

=> 3.0

Since POINT is mutable, we can efficiently modify its fields using
SET-DATA.

> (defun mirror-point! (pt)
    (adt:with-data (rectangular x y) pt
      (adt:set-data pt (rectangular y x))))

> (let ((pt (rectangular 1.0 2.0)))
   (mirror-point! pt)
   (adt:match point pt
     ((rectangular x y) (format t "point is (~A, ~A)" x y))
     (_ nil))

will print "point is (2.0, 1.0)".

See examples.txt for examples.


Frequently Asked Questions
--------------------------

Q. How do we define KAR and KDR for LISZT?

A. Easy.

(defun kar (l)
  (adt:match liszt l
    ((kons a _) a)
    (knil knil)))

(defun kdr (l)
  (adt:match liszt l
    ((kons _ b) b)
    (knil knil)))


Q. Can we do parametric ADTs like I can in Haskell?

A. Not unless you want things to be inefficient.


Q. Why doesn't deeper pattern matching work?

A. It's not implemented, and it would be hard to implement.


Q. Can I get the constructors dynamically for a particular ADT?

A. Yes. You can get the constructors and associated arity by calling
the GET-CONSTRUCTORS function, which will return a list of
(constructor arity) pairs. For example, given the LISZT example above, we have

CL-USER> (adt:get-constructors 'liszt)
((KONS 2) (KNIL 0))
T

The T represents the fact that the ADT is known and exists.