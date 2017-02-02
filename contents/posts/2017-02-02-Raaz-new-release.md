---
title: "Raaz: New release"
tags: Raaz, Cryptography, Haskell
---

I have just uploaded, on hackage, a [new release candidate][raaz-0.1.0-candidate] for
[raaz]. I plan to release it on Feb 28, 2017 (coinciding with the
[wikipedia:National Science Day]() in India).

- [Change log][changelog]
- [Candidate release page][raaz-0.1.0-candidate] on hackage.


This release include the following primitives

- [chacha20] stream cipher

- a prg based on [chacha20]

Besides the portable C implementation, we have an implementation that
uses 256bit vector instructions (using GCC/Clang intrinsics). The
randomness source for this library uses the chachac20 cipher as a
PRG. The stream cipher is really efficient with the following figures
on my machine[^spec]

* Encryption/decryption:
     - portable C: 4.98 Gbps
     - vector256+avx2: 9.55 Gbps

* Pseudo-random bytes: 5.45 Gbps

Notice that the vector256+avx implementation can pretty much saturate
a 10Gbps line. The PRG uses the most efficient [chacha20] implementation
available, which in this case is vector256+avx2. The reduction in
performance for prg is due to some copying overhead that I have not
bothered to optimise. For more details and comparisons with other
primitives see the gist:

<https://gist.github.com/piyush-kurur/93955e669ab72a51996590bfc106677d>


[chacha20]: <https://tools.ietf.org/html/rfc7539> "ChaCha20 RFC"
[raaz]:      <https://github.com/raaz-crypto/raaz/>
[changelog]: <https://github.com/raaz-crypto/raaz/blob/release-0.1.0/CHANGELOG.md> "Change log for 0.1.0"
[raaz-0.1.0-candidate]: <https://hackage.haskell.org/package/raaz-0.1.0/candidate>
[mighttpd]: <http://mew.org/~kazu/proj/mighttpd/en/>
[satvik]: <http://github.com/satvikc>
[rabisg]: <http://github.com/rabisg>
[green-threads]: <http://en.wikipedia.org/wiki/Green_threads> "Wikipedia:Green threads"
[stm]: <http://www.haskell.org/haskellwiki/Software_transactional_memory>
[mvar]: <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html>
[^spec]: Intel i7-4770 CPU @ 3.40GHz x 8 cores.
