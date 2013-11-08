---
title: Travis builds for Raaz
tags: Raaz
---

This post describes the structure of [travis] CI system in place for
[raaz], the cryptographic network library for [Haskell] that we are
developing. This documents some of its tricky aspects and what more
needs to be done.

If you are not familiar with [travis] CI please check it out. The
tight integration of [travis] with [github] means every push to the
repository is built and checked. What more, the system also builds
every pull requests, so before merging I can be sure that stuff
works. Having said that there are certain limitation of [travis]
builds particularly in the context of [raaz] which I list below.


1. The travis builds are done on an [Ubuntu] container. Therefore, the
   default builds are against whatever haskell platform comes with it.
   Thanks to [Herbert Valerio Riedel's][hvr] for his
   [write up][hvr-instructions] and his [Ubuntu ppa][hvr-ppa], we now
   build across multiple haskell platform. The instructions need to be
   tweaked though as they assume that repository is a single directory
   with the package in the root directory. I have described this in
   the next section. This therefore is mostly solved.

2. Since I myself use [Debian] stable and [travis] uses [Ubuntu],
   there is not much cross OS builds. Ideally I would want it to be
   built on at least few of the BSD variants.


3. The builds happen only on one architecture, the architecture that
   the [travis] build runs on. While this is not a problem for most
   packages, [raaz] being a cryptographic library should be built
   across multiple architecture. Only then can we catch bugs due to
   endian mismatch, alignment problems (ARM in particular). This
   becomes all the more important when we start including architecture
   specific implementations of primitives.

I think (2) should be easy to solve. Someone with more BSD experience
can help out on this. I think (3) is particularly difficult because we
need actual hardware to test it out. One option would be to build it
and run on an emulator like [qemu]. However, I do not know of any
build system that makes this easy. On the other hand OS distributions
like [Debian] should have solved this problem. I would like advice
from some knowledgeable person here.

# Multi-platform build: Raaz's idiosyncracies.

The [raaz] repository is a collection of haskell packages with
dependencies between them. So
[Riedel's instructions][hvr-instructions] do not work directly. We
need to make sure that

1. To build a package we need to be in its root directory. This
   involves cd'ing into the directory and essentially following
   [Riedel's instructions][hvr-instructions]

2. Before installing a package like
   [ `raaz-hash-sha` ][raaz-hash-sha], we need to install all its
   dependencies within the raaz collection as those packages are not
   on cabal. We use `make` to ensure these.

I have documented most of this in the [ `Makefile` ][travis-makefile]
which you might want to refer to. Let me know if the documentation
needs improvements.

## Package version subtleties.

There is a subtle problem with dependencies that makes multi-platform
builds more or less meaningless. The way we ensure that the packages
are built against a platform is by setting up an appropriate
`cabal.config` file in the directory of the package. This config file
puts constraints corresponding to the platform we want to test
against.  I noticed that the builds are actually not installing the
platform packages because of the version dependency. For example, the
dependency on Quickcheck that we had was `Quickcheck==2.4.*`. This
means that even for builds for [platform 2013.2.0.0][haskell-platform]
the quick package used was `Quickcheck-2.4.something`. For this reason
we had to go for a more liberal package version bounds (merge
[ac0ad7][merge-version-bumps]). As long as we are not using the
packages like `base` or `Quickcheck` in a non-standard way, I think we
are fine.


[haskell-platform]: <https://www.haskell.org/platform/changelog.html>

[travis-makefile]: <https://github.com/piyush-kurur/raaz/blob/master/Makefile>

[raaz-hash-sha]: <https://github.com/piyush-kurur/raaz/blob/master/raaz-hash-sha>

[hvr]: <https://github.com/hvr> "Herbert Valerio Riedel"
[hvr-ppa]:
    <https://launchpad.net/~hvr/+archive/ghc>
    "Herbert V Riedel's Ubuntu PPA"

[hvr-instructions]: <https://github.com/hvr/multi-ghc-travis>

[merge-version-bumps]:
   <https://github.com/piyush-kurur/raaz/commit/ac0ad7afd711b656a77257ebc4ba923cb939b3f3>

[qemu]: <http://www.qemu.org> "QEMU processor emulator"
