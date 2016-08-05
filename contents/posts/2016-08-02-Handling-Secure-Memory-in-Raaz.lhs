---
title: Handling secure memory in Raaz.
tags: Raaz, Cryptography, Haskell
---

# The problem

Cryptographic software need to keep confidential data like private
keys in its memory. Unless the machine is severly compromised, this is
safe as no other process can access them and the data vaporises once
the power is turned off. However, if the operating system swaps the
data into external memory (hard disk) during the execution of the
program, the secret data gets stored on permanent memory and can
remain live for years to come. Therefore it is clear that one needs to
prevent the OS from swapping out data that is sensitive and typical
operating systems provide system calls `mlock/munlock` which
locks/unlocks memory from being swapped. The library should lock all
the memory that contains sensitive data and after use should wipe the
memory clean before unlocking and freeing.



## A "solution" using [`ForeignPtr`].

The [`ForeignPtr`] type in Haskell is a pointer together with a
finalisation routine. When the pointer goes out of scope, the
finalisation routine is run before the memory is de-allocated. A naive
solution for storing sensitive data is to store it in a _locked_
[`ForeignPtr`] based buffer.  The finalisation step of this foreign
pointer should wipe the memory clean and unlock it. This seamingly
easy solution has the following problem.

The system calls `mlock`/`munlock` works at the level of pages,
i.e. it locks or unlocks multiple of pages. Now consider two locked
buffers `b₁` and `b₂` where `b₁` ends at the first byte of a page and
`b₂` is the rest of the page. Then unlocking `b₁` will also unlock
`b₂` as `munlock` unlocks all the pages that contains part of `b₁`. If
`b₁` and `b₂` are foriegn pointers, then when the gc notices that `b₁`
gets out of scope it will unlock `b₁` as well as `b₂` (because the
entire page is unlocked). In other words nesting of mlock and munlock
does not work. This clearly is not acceptable.

Of course this idea can be made to work by building a mini-garbage
collecter inside the crypto-library. We should maintain a pool of
_locked_ memory pages and [`ForeignPtr`]'s meant to be used for
sensitive data should be allocated from this pool. The finaliser of
the these [`ForeignPtr`]'s do not unlock immediately on going out of
scope but merely mark that a given chunk of memory is unused.  A
particular page can be unlocked only when no portion of it is part of
any live secure foreign pointers. With some book-keeping such an
allocator can be built but it is tricky. We need to take care of all
the issues related to garbage collection like fragmentation besides
knowing system level details like page sizes. An ancient version of
raaz had such a memory allocator and it was not pretty.

## Raaz's simplified memory model.

The main idea behind the secure memory interface is that we allocate
all the required secure memory in one go. This approach is faster and
simpler. We describe this interface here.

In Raaz we have the notion of an abstract memory element which are
buffers wrapped in an appropriate type. These are instances of the
class [`Memory`].

>
> class  Memory mem where
>   memoryAlloc    ::  Alloc mem       --  allocation strategy
>   underlyingPtr  ::  mem -> Pointer  --  recover the pointer
>

The type [`Alloc mem`][Alloc] captures what we call an _allocation
strategy_ for the memory element `mem`. This is essentially a pair
`(Pointer -> mem, Int)` which encodes the following:

1. The `Int` portion keeps track of the amount of memory that is
   required to create the memory element `mem`.

2. The `Pointer -> mem` part gives the constructor for the memory
  element, i.e. it gives a function that takes a pointer which points to a
  block of memory, and create the
  memory element.

All actions that require secure memory should be of the type `mem ->
IO a` for some memory element `mem`. In the library, this is captured
by the type [`MT mem a`][MT]. It is easy to see that one can easily
define a higher order function [`securely :: MT mem a -> IO
a`][securely] that takes such a memory action and passes it a memory
element by constructing it out of a locked memory of appropriate
size. At the end of the action, this combinator also ensures that the
memory is wiped clean before unlocking the memory. Such a use which
involves a single mlock/munlock call is not problematice. Besides we
do not need to know any system dependent parameters.

What about more complicated actions action that requires many such
memory elements, say for example `mem1` and `mem2`?  We think of it as
an action that takes the pair `(mem1, mem2)`. This requires us to
define a memory instance for product types which becomes too tedious
because of the pointer arithmetic and size calculation involved in
defining its allocation strategy. Every such low level code has the
word disaster written all around it.

It turns out that an [`Applicative`] functor instance can be defined
on the type [`Alloc`][Alloc] _which does the right thing_.  The
allocation strategy for the compound type (a product of simpler memory
types) can be constructed out of the allocation strategy of its
components using this applicative interface. The [`Memory`] instance
of a product type will then be something along this lines:

> instance (Memory mem1, Memory mem2) => Memory (mem1, mem2) where
>     memoryAlloc         = (,) <$> memoryAlloc <*> memoryAlloc
>     underlyingPtr (m,_) = underlyingPtr m
>

Note that all the book keeping involved in the length calculations and
pointer arithmetic is hidden in the applicative interface. All
Implementations of primitives in Raaz always use a memory element to
keep its internal state secure.


To bootstrap the process, the library provides some basic memory types
like [`MemoryCell`]. Compound memory types (which are essentially
product of simpler memory types) can be built out of them using this
[`Applicative`] instance of [`Alloc`][Alloc].

## Does it still keep things from swapping out?

Unfortunately, there can still be instances where things can go
outside secure memory. For example, if you reads the contents of a
memory cell into a pure value of Haskell, the contents have leaked
into the Haskell Heap which might be swapped. However, with care we
can minimise such explicit reads. Copying from one memory cell to
another can be done using a memcpy which does not involve such
transfer to the heap.

For the actual implementation see the documentation of the module
[Raaz.Core.Memory][raaz-memory]. The theory behind the applicative
structure of [`Alloc`][Alloc] is dealt with in [our upcoming paper in
Haskell 2016][twist-pointers].


[twist-pointers]: </research/publication/Conference/2016-09-22-How-to-twist-pointers.pdf> "How to twist pointer without breaking them"

[raaz-memory]:
	<https://hackage.haskell.org/package/raaz-0.0.2/docs/Raaz-Core-Memory.html>
	"Raaz: Secure memory"

[securely]:
	<https://hackage.haskell.org/package/raaz-0.0.2/docs/Raaz-Core-Memory.html#v:securely>
	"running the memory action securely"
[MT]:
	<https://hackage.haskell.org/package/raaz-0.0.2/docs/Raaz-Core-Memory.html#t:MT>
	"Memory action"

[Alloc]:
	<https://hackage.haskell.org/package/raaz-0.0.2/docs/Raaz-Core-Memory.html#t:Alloc>
	"Allocation strategy"

[`Memory`]:
	<https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Memory.html#t:Memory>
	"The Memory type class"

[`MemoryCell`]:
	<https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Memory.html#t:MemoryCell>
	"The Memory type class"

[`ForeignPtr`]: <https://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-ForeignPtr.html#t:ForeignPtr>
        "Foreign Pointer"
[`Applicative`]: <https://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Applicative.html#t:Applicative>
