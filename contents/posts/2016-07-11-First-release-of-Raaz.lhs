---
title: First release of Raaz
tags: Raaz, Cryptography, Haskell
---

After a long delay, and a lot of false detours, I released the very
first version of [Raaz], a cryptographic network library for
[Haskell]. My reasons on embarking on such a project is briefly
mentioned in a [previous post][motivation].


The current release is mostly a proof of concept release with very
little primitives; we currently support sha1, sha2 hashes and their
hmacs. We also have an implementation of aes-cbs but these are hard to
use in an high level fashion.

# Quick examples.

We demonstrate the interface for computing message digests based of
cryptographic hashes. Raaz uses distinct types for distinct
hashes. These types are opaque in the sense that we do not expose its
constructor. However, they are instances of the `IsString` and `Show`
class which makes them conviently representable in code.

> {-# LANGUAGE OverloadedStrings #-}
> import Data.ByteString.Char8(ByteString)
>

We now demonstrate how the sha512 digest of a file can be computed.
All cryptographic hashes are instance of the type class `Hash` and the
generic function [`hashFile :: Hash h => FilePath -> h`][`hashFile`]
can be used to compute the hash of a file. The top level signature is
necessary in this case so that ghc can deduce which hash to compute.

> fileDst :: IO SHA512
> fileDst = hashFile "myphoto.jpg"

## Entering hashes directly in source code.

With the `OverloadedStrings` extension, you can enter the hashes
directly in the source code using the string notation. However, for
objects that do not have 1:1 correspondence with byte strings, use of
this extension often lead to runtime errors and its use is generally
_not recommended_. There are some rarely cases when it is convenient
to embedd hash values in source code like when writing unit tests.

> emptySHA256 :: SHA256
> emptySHA256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

Hashes can be compared for equality. What more, these comparison are
constant time and hence timing safe.

> check :: Bool
> check = emptySHA256 == hash ("" :: ByteString)

Notice that for pure values like ByteString, we use function [`hash`]
to compute its digest. The result is also a pure value.

For more details, we refer the reader to the [haddock documentation of
`Raaz.Hash`][raaz-hash]

# Design philosopy.

The raaz package will expose cryptographic primitives which will be
used to implement specific cryptographic protocols (in separate
packages). We would like to expose a high-level view of cryptographic
primitives and levarage the type safety of Haskell when ever
possible. Here are some design principles that we have followed which
we believe are important.

**Types distinguish functionality:**
:   In cryptographic literature, data like hashes are often treated just
    as a sequece of bytes.  For example, a string of 32 bytes, a sha256 hash
    or its hmac or for that matter any other hash like blake2s are all
    just 32-byte strings. For a crypto library this is a bad design. This
    is confusing the semantics of the data type with is syntax (encoding)

    Raaz makes these types distinct and, to prevent accidental usage
    in ways unindented, makes these types opaque.

**Encoding:**
:   We do care about external presentation of cryptographic data captured
    by the data class [`Encodable`]. Almost all cryptographic data that has
    some encodable presentation is an instance of this type. An instance
    of [`Encodable`] can be converted to any of the supported binary formats.
    Currently only hex-encoding is supported but the git repository has
    Base64 encoding. The design of the encoding interface is done and adding
    a new encoding should not be difficult.


**Equality in constant time:**
:   We ensure that all cryptographically sensitive data, like hashes
    have constant time equality comparisons, i.e. the time does not depend
    on whether the values are equal or not or by how many characters they
    match. We expose a class [`Equality`] which aids in constructing such
    timing safe equality.

**Multiple low level implementation:**
:   For cryptographic primitives, we support the use of multiple implementations.
    One of the goal of the design is to allow users to plugin their favorite
    implementation if the default one provided by the library was found lacking.
    However, this is an advanced feature and may need quite a bit of hacking.

**Secure memory interface:**
:   We have an interface for secure memory, i.e. memory that is locked and will
    not be swapped. The library ensures that this memory is wiped clean after use.
    All implementations are designed to use this secure memory to store their sensitive
    internal state. See the module  [`Raaz.Core.Memory`][raaz-memory] in the library.

**Documentation:**
:   We hope to have a well documented cryptographic library. In fact,
    quite a few students from IIT Kanpur have contributed to this
    project. I hope there is enough documentation in the source so that
    students can easily start working on it (as an academic I would like
    students to participate in such development efforts).


For more details visit

1. [Our Repository][repo]

2. [Haddock documentation][raaz-haddock]

[raaz-hash]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html> "Documentation for Hashes in Raaz"
[raaz-memory]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Memory.html> "Raaz: Secure memory"
[`Equality`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Types.html#t:Equality> "Timing safe equality"
[`Encodable`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-Encode.html#t:Encodable> "The class Encodable"
[`hash`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html#t:hash> "The hash function"
[`hashFile`]: <https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Hash.html#t:hashFile> "The hashFile function"
[raaz-haddock]: <https://hackage.haskell.org/package/raaz-0.0.1/> "Haddock documentation for Raaz"
[motivation]: </posts/2013-08-24-Raaz-A-cryptographic-network-library.html>
[repo]: <https://github.com/raaz-crypto/raaz>
