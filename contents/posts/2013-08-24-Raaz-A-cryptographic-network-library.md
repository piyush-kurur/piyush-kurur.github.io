---
title: Raaz: A cryptographic network library for Haskell
tags: Raaz, Cryptography, Haskell
---

This is my first post on [Raaz], a cryptographic network library for
[Haskell]. [Raaz] broadly aims at developing into:

1. A platform to experiment with various cryptographic primitives.

2. A library to write high performance servers and clients to some
   common cryptographic network protocols.

I believe that [Haskell] as a language has a lot of features that
allow writing fast (as fast or better than any [C] library available)
as well as secure cryptographic code. In this post, I attempt to
explain some of the features of [Haskell] that we make use of.

## Speed

Let me first dispose of the one myth that seems to persist in the mind
of people who have never seen a modern functional language. No one
wants their software to be slow. Cryptographic protocols should be
especially well implemented otherwise folks would simply avoid using
the secure options. Clearly when it comes to performance [Haskell] can
beat any of the interpreted languages [Python], [Ruby] or [Java]. But
what about [C]?

The tight loops in the library which implements the primitives will
anyway be written in [C]/Assembly. If one wants speed then one needs
to do this whether one likes it or not. So for primitives it really
does not matter which language one chooses. It then boils down to how
easy it is to integrate [C]/Assembly code with [Haskell]. Having a low
overhead foreign function Interface (FFI) is really critical here and
Haskell fortunately has it.

Having fast primitives helps but a network library is not just a set
of fast cryptographic primitives. Here are some of the features that
one would.

1. High performance concurrency primitives for server
   applications. [Haskell] really has no competition in this
   department. Here are some of the features that GHC (and libraries)
   supports: [wikipedia:Green threads]()
   [STM]s, [MVar]s etc. Using these features, servers written in
   [Haskell] have been competitive (often outperforming) servers
   written is [C]. See for example [mighttpd].


2. Efficient data serialisation and parsing libraries: Implementing
   the wire protocol efficiently is critical in improving the
   efficiency of the network application. [Haskell] is especially rich
   in this department as well: [attoparsec], [binary], [blaze-builder]
   etc. There are libraries that supports high performance (close to
   hand written [C] performance) at the same time achieving these
   feats at a much higher level of abstraction (which translates to
   less bugs and high maintainability).

While having fast libraries is great, languages like [C] achieve this
at the cost of abstraction. It often appears to the programmer that
one needs to sacrifice elegance for speed. Not so with [Haskell]. Many
of the libraries I mentioned above achieve [C] speed with no
compromise on the level of abstraction. This greatly enhances the
maintainability and leads us to the next important feature that we
want in our libraries, safety.

## Safety.

Cryptographic implementations are full of corner cases and the bugs in
them can be particularly lethal. A cryptographic library is usually
broken, not by a direct attack on the underlying algorithm, RSA
although quite dated is still secure, but through other means like
buffer overflows, cache timing attacks and other *side channel*
attacks.  How can one minimise this? Let me give an example of a code
which, while correct in normal circumstances, is bad in a crypto
setting. Suppose you grant privileged access to a user by comparing a
secret that you posses with the user supplied password. A naive string
comparison will be prone to timing attacks: The time taken to reject a
password is proportional to length of the longest common prefix of the
secret and the password. The attacker then can guess the password one
character at a time by looking at the time it takes for you to reject
the password. One would usually not compare the secrets directly but
hash them together with a salt and the hashes. However, any
comparisons that take time dependent on the user input is prone to
lead to future attacks when deployed without much thought.

We could avoid this problem by asking users of our library to always
use string comparisons that take constant time irrespective of the
input. However, it is very likely that a user of our library, most of
them will not be cryptographers, might miss this instruction. Won't it
be nice if such incidents are caught at compile time?

We avoid this problem in [Haskell] by leveraging its type safety.
Instead of representing cryptographically significant data types like
hashes, macs etc. as mere byte string, we define [Haskell] data types
for them. For example sha1 hashes are represented (in a simplified
form) as follows:

~~~{ .haskell}

module Raaz.Hash.Sha1 ( Sha1 )

data Sha1 = Sha1 Word32 Word32 Word32 Word32 Word32

instance Eq Sha1 where
	(==) (Sha1 h0 h1 h2 h3 h4) (Sha1 g0 g1 g2 g3 g4)
             =   xor h0 g0
             .|. xor h1 g1
             .|. xor h2 g2
             .|. xor h3 g3
             .|. xor h4 g4
             == 0
~~~

The `Eq` instance for Sha1 has comparison operator defined in such a
way that it will take time independent on the number of positions they
match. A user is then be forced by the compiler to use this equality
as we will not be exposing the constructor to her.

## Status of the project and how to contribute

Currently we have just began. We have made no releases yet and we are
still experimenting with the API. All code is available under BSD3
license from <http://github.com/piyush-kurur/raaz>).

I look forward to your contributions. In particular, if computer
architecture is your bread and butter and you are the
[wikipedia:Chuck Norris]() of assembly language programming, do join us
for some fun coding: A lot of primitives require fast implementation
often exploiting the platform specific features like [wikipedia:SIMD]()
instruction set.


[mighttpd]: <http://mew.org/~kazu/proj/mighttpd/en/>
[satvik]: <http://github.com/satvikc>
[rabisg]: <http://github.com/rabisg>
[green-threads]: <http://en.wikipedia.org/wiki/Green_threads> "Wikipedia:Green threads"
[stm]: <http://www.haskell.org/haskellwiki/Software_transactional_memory>
[mvar]: <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html>
