---
title: Beware of numeric type classes.
tags: Haskell
---

This blog post is to document some cases where unconstrained Num type
definitons can seriously compromise type safety. None of what I say
here is new but it is worth repeating. Take it as a note of warning
against being zealous when defining instances of some standard type
classes, in particular the `Num` class

Here is an example from the cryptographic library `Raaz`. To prevent
inadvertent use of the wrong offset measure, raaz uses what we call
type-safe lengths. One such length is `BYTES` but there are
others. For example, the type `BLOCKS c` measures length in multiples
of the block length of the the cipher `c`. The advantage of such a
definition is that for low level functions like `encryptBlocks` the
length argument is in `BLOCKS c` which ensures that one does not
accidently supply length in bytes. The constructors of BLOCKS should
not be directy availabe to the outside world to avoid users from
circumventing such gurantees. Instead we define a class `LengthUnit`
which has a member function that converts instances to `BYTES`. This
instance function is what is use to perform the actual size
calculation of pointer manipulation.

It is tempting to define the `Num` instance for `BLOCKS` as this is a
great convenience for users. However, such an instance will effectively
bypass all the type safety we so carefully built into the
system. Consider for example, a code like `encryptBlocks chacha20imp
ptr 42`. Haskell is able to infer that the 42 is of type `BLOCKS
ChaCha20` because it knows the type of `chacha20imp`. This will not
raise any kind of type error and will result in encrypting 42 blocks
(i.e. 2688 bytes) of data. This is a disaster if the user actually
meant only `42` bytes. Haskell should not have the privilege of such
bugs unlike others like Fortran (See [wikipedia:Mariner 1]()).

A theoretical reason to see why the `Num` instance is plain wrong for
length units is the see that multiplication does not make sense
here. In the language of physics, length offset is _not_ a
dimensionless scalar and hence the product of $6\mathrm{bytes} × 7
\mathrm{bytes}$ is $42 \mathrm{bytes}^2$ which is not a length unit at
all. On the other hand, $10 * 4 \mathrm{ bytes}$ makes sense when
calculating the size of say an array of `Word32` of length 10. This is
because the quantity 10 is a _dimensionless_ and therefore the
resulting product has the same dimension as bytes.

In the heat of the moment it is pretty easy to overlook these
bugs. This is particularly tempting because of the "unnecessary"
boilerplate that it gets rid of that to at very minimal cost; just a
`deriving` clause away. I would freely confess my stupidity in this
aspect. At least we have
[repented](https://github.com/raaz-crypto/raaz/issues/247) and
[corrected](https://github.com/raaz-crypto/raaz/pull/251) this to
avoid perdition.
