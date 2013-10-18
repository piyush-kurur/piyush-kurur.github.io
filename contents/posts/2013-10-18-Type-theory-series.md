---
title: Types, proofs and programming.
tags: Type theory
---

For quite some time, I have been toying with the idea of trying to get
a grip on the various beautiful ideas that go under the name *type
theory*. It looks to me that the time has come for a deep dive into
the [hott] (no this is definitely not a typo) waters. This is the
first in the series of posts that on type theory and its various
ramifications to Functional programming and foundations of
mathematics. I am not an expert in this area but by blogging I hope to
gain the insights that has eluded me so far.

This particular blog post also will serve as a "Table of Contents" for
the series. While all the posts in this series will have the
"[Type theory]" tag in it, the explicit listing of the contents serves
as a suggested order in which to read the posts.

# Perspective.

In a sufficiently rich functional programming language (like for
example Haskell), we have *expressions* each of which are associated
with a *type*. The types can be seen as *invariants* that the
expressions satisfy through out the program. For example, when you
assert that the variable `x` has type `Int`, you are implicitly asking
the compiler to make sure that `x` is used in ways that are consistent
to the fact that it is an integer. Checking this invariant at compile
time ensures that the programmer does not introduce silly bugs like
trying to add `2` to `"hello"` etc.  Thus there is some amount of
*theorem proving*, rather trivial in the above case, already built
into any strongly typed language. This informal connection can be
formalised via what is know as the Curry-Howard Isomorphism which
observes that the rules for assigning well defined types to
expressions coincide remarkably with rules for proving statements in a
suitable logic. For any type $τ$ one can associate a statement $A_τ$
such that if $e$ is a well typed expression of type $τ$, we can map
$e$, rather the derivation of the type of $e$, to a proof of the
statement $A_τ$. The precise statement of this connection is left for
later posts but the core idea here is that *type checking* is
essentially *proof checking* (and vice-versa).

Why is this connection interesting? In order to fully see the
[wikipedia:Blind men and an elephant|type theory elephant](), we often
need to acquire the split personalities of a programmer (functional
programmer) and a mathematician. The programmer is actually interested
in the expressions, as they are the programs. Types are a way of
ensuring that the programs satisfy certain invariants. The stronger
the type system, the larger is the class of invariants that can be
expressed. Thus for her the types are a means to achieve correct
programs. This also means that from a programmers perspective
efficient implementation of the language is more important.

The mathematician is more interested in the types as they correspond
to mathematical truths. Expressions are just proofs of these
statements. The functional programming language with sufficiently
powerful types can thus be used as a proof assistant to achieve
completely automating mathematical proof checking. This viewpoint
means that a mathematician is not that much concerned about efficiency.

[type theory]: </posts/tags/Type theory.html> "Posts on Type theory"
[hott]: <http://homotopytypetheory.org> "Homotopic Type Theory"
