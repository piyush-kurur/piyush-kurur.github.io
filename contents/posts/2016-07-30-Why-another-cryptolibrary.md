---
title: Why another cryptolibrary?
tags: Raaz, Cryptography, Haskell
---

Haskell is already endowed with a comprehensive set of libraries for
cryptography like [cryptonite], for example. What justifies working on
another cryptographic library, after all isn't the 11-th commandment
"Thou shall not (re)-implement cryptography" ? This post is _not_ a
[cryptonite] vs [raaz] comparison --- a simple look at the haddock
documentations of the two libraries should convince anyone that
[cryptonite] is far ahead of [raaz]. This post is an attempt at
bringing out the reasons for working on [raaz] and what is the rough
road map that I intend to follow. But firstly I would like to dispose
of two myths that seems to be prevalent on the internet at large when
it comes to cryptographic library.

Firstly, I would like to dispose of the unnecessary fear of
(re)-implementing cryptography. This fear is unfounded and, I believe,
is partly responsible for open source libraries like OpenSSL becoming
the "enterprise" level crypto-implementation that it is now. How else
does one explain a successful open source project becoming such a
nightmare of a code base?  Cryptographic libraries are software
libraries in the first place so all the best practices for building
large scale software should be applicable to it as well. How can it be
otherwise?  I am not denying the fact that there are certain class of
bugs unique to cryptographic implementations like timing based
attacks. However, once these class of bugs are identified, it is just
a question of suitably modifying the development process to account
for such bugs as well. As a Haskell programmer, one should be
wondering how to exploit the type system to avoid these kinds of bugs.

The second myth is that one needs to be a professional cryptographer
to write a cryptographic library. Granted, it is difficult if one is
[Donald Trump] but one does not need to be [djb] either to pull it
off, although being the later does give some significant
advantages. Building software in any field requires absorbing the
nuances of the field and cryptography is no different. To drive home
the point, it would be absurd to say that one should not work on an
api to access databases just because one is not a database expert.

A professional cryptographer has an advantage but remember, she might
not have experience building large software and might not be familiar
with some of the cool tricks one can play with types.  So if you think
you are capable of developing complex software, with enough background
reading and awareness of the literature, you should be able to
contribute significantly in the development of a crypto-library.

I am _not_ a professional cryptographer by any means. One of my
current interest is to build reliable software by using formal methods
whenever possible.  With a strongly typed language like Haskell, I
believe, one can go quite a bit in building a secure cryptographic
library. This is my main motivation behind [Raaz]. The kind of bugs
that we want to deal with are

1. Timing related bugs.

2. Bugs due to secret leaking out of unlocked memory.

3. Bugs arising due to low level buffer management.

[Raaz] addresses all these in its own unique ways.

## Timing attacks.

While timing related bugs are hard in general, for example
cache-timing attacks for AES SBOX (current version of raaz uses an
sbox), some trivial kind of timing attacks like the ones due to naive
string comparisons should be avoided whenever possible. In a language
like C, one does not have much hope here. Even if the library writer
is careful enough to provide a timing independent string comparison,
the enforcement of this is left at the whims and fancies of the
application developer.

[Raaz] provides the class [`Equality`], the timing independent cousin
of the `Eq` class. Instead of the the function `(==) :: Eq a => a -> a
-> Bool` what we have is the function `eq : Equality a => a -> a ->
Result`. Here the `Result` type is an opaque type that captures the
result of a comparison. Two such comparisons can be combined using the
monoid instance of `Result` which essentially takes the AND of the two
results but does it in a timing safe way.

In [raaz] we insist that cryptographically sensitive types like hashes
and MACs should have their `Equality` instance declared first. Their
`Eq` instance is then declared using the combinator
[`(===)`][triple-eq] which makes use of `eq` to do the timing safe
comparison. Clearly, we cannot enforce this at compile time but we can
look for this pattern while reviewing the code base. Anything other
than this, like for example a `deriving Eq` clause, should raise
suspicion.

## Pointer manipulation.

The library needs a lot of low-level pointer manipulation in
serialisation/de-serialisation and memory allocation. We have an
abstraction for such pointer manipulation as a result of which,
pointer arithmetic is done only once in the entire code base. The
abstraction is based on a generalisation of semi-direct product and is
described in our upcoming
[Haskell symposium paper][twist-pointers]. In particular, our secure
memory interface uses this abstraction.

## Known limitations and future directions.

My first aim is to get the API correct. We have place holder
implementations of the SHA2 family of hashes and aes-cbc but we have
not really tweaked these either for security or performance. The AES
code for example uses sbox which is [not really a great idea][aes-cache-timing].

In no particular order these are the current goals (comments,
criticism and most importantly pull requests are welcome).

1. High quality documentation (both haddock and internal source code
   level documentation).

2. Using types every where. The types should work for us even at the
   lowest of the levels at which we work.

3. Formalising a reviewer's check list which contains things to
   actively look for while reviewing code. The example of the
   `Equality` instance is one such.

4. Building against multiple architecture. This is to detect bugs due
   to alignment restrictions, endianness confusion etc. One easy way
   to get this working is to build on Ubuntu launch pad.

It does not mean I will be unhappy with merging a
[bit-sliced implementation of AES-CTR][aes-without-sbox] but the above
ones are my priorities as of now.

[cryptonite]:
	<https://hackage.haskell.org/package/cryptonite>
	"Cryptonite: Cryptography Primitives sink"

[raaz-hash]:
	<https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html>
	"Documentation for Hashes in Raaz"

[raaz-memory]:
	<https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Memory.html>
	"Raaz: Secure memory"

[aes-cache-timing]: <http://cr.yp.to/antiforgery/cachetiming-20050414.pdf>

[aes-without-sbox]: <http://crypto.stackexchange.com/questions/55/known-methods-for-constant-time-table-free-aes-implementation-using-standard>
[Donald Trump]: <https://en.wikipedia.org/wiki/Donald_Trump> "Donald Trum"
[djb]:<http://cr.yp.to/djb.html> "Daniel J Bernstein"
[`Equality`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Types.html#t:Equality> "Timing safe equality"
[`Encodable`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Encode.html#t:Encodable> "The class Encodable"
[`hash`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html#t:hash> "The hash function"
[`hashFile`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html#t:hashFile> "The hashFile function"


[raaz-haddock]: <https://hackage.haskell.org/package/raaz-0.0.1/> "Haddock documentation for Raaz"
[motivation]: </posts/2013-08-24-Raaz-A-cryptographic-network-library.html>
[repo]: <https://github.com/raaz-crypto/raaz>
[raaz-hash]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html> "Documentation for Hashes in Raaz"
[raaz-memory]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Memory.html> "Raaz: Secure memory"
[`Equality`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Types.html#t:Equality> "Timing safe equality"
[triple-eq]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Types.html#v:-61--61--61-> "The (===) operator"
[`Encodable`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Encode.html#t:Encodable> "The class Encodable"
[`hash`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html#t:hash> "The hash function"
[`hashFile`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html#t:hashFile> "The hashFile function"
[raaz-haddock]: <https://hackage.haskell.org/package/raaz-0.0.1/> "Haddock documentation for Raaz"
[motivation]: </posts/2013-08-24-Raaz-A-cryptographic-network-library.html>
[repo]: <https://github.com/raaz-crypto/raaz>
[twist-pointers]: <research/publication/Conference/2016-09-22-How-to-twist-pointers.pdf> "How to twist pointer without breaking them"
