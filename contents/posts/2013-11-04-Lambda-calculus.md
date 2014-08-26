---
title: Lambda calculus
tags: Type theory
prev: /posts/2013-10-18-Type-theory-series.html
next: /posts/2014-01-11-Typed-lambda-calculus.html
---

We begin the exploration of type theory by describing lambda
calculus. This post is mostly to set up the notation and the standard
reference for material here is the encyclopedic book
[The lambda calculus: Its syntax and semantics][lambda calculus book].
Also have a look on [wikipedia:Lambda calculus|Wikipedia]() for
details and references.

The core idea of lambda calculus is an elegant notation invented by
[wikipedia:Alonzo Church]() to capture functions without naming them.
Consider the function that increments its argument by 1. In notation
of lambda calculus one can write such an increment function as $λ x
. x + 1$ (assuming of course that $+$ has already been defined). This
notation has now found its way to many programming languages like
Haskell (`\ x -> x + 1` the backslash is an [wikipedia:ASCII]()
approximation of $λ$), python (`lambda x: x + 1`) etc. The lambda
calculus we deal with here does not have the *built-in* function like
$+$. All it has is a (countably infinite) supply of variables, which
we will denote by small case letters $x$, $y$, $z$ etc and two
fundamental operation namely application and abstraction. The lambda
calculus expressions can be inductively defined as follows:

$$ e = x | e_1 e_2 | λ x . e$$

In the above inductive definition $x$ is any one of the countably
infinite variables and $e_1$ and $e_2$ are lambda calculus expressions
defined recursively. We will follow that standard convention that all
application associates to the left (i.e. $f g h$ means $((f g) h)$)
and that application binds tighter than $λ$-abstraction. Further, the
the expression $λxy.xy$ means $λx . λy . xy$.

# Free and bound variables.

The lambda abstraction acts like any other mathematical quantifier
when it comes to determining the free and bound variables. An
occurrence of a variable in a expression is either free (i.e. is not in
the scope of any $λ$-abstraction) or is bound to a lambda
abstraction. If an occurrence of a variable $x$ is in the scope of more
than one $λ$-abstraction then it is bound to the inner most one.  For
example in the expression $λx.xy$, $x$ is bound and $y$ is free. One
can define the notion of free variables inductively as follows.

$$FV(x)       = \{ x\},$$
$$FV(e_1 e_2) = FV(e_1) ∪ FV(e_2),$$
$$FV(λx . e)  = FV(e) ∖ \{x \}.$$

As in other mathematical notations, the meaning of a lambda calculus
expression depends only on the free variables, which means one can
change the bound variables of an expression. For example the
expression $λx . xy$ is the same as $λt. t y$. This change of bound
variables is called $α$-conversion. When doing an $α$-conversion
however, care must be taken to avoid variable bindings to be
inadvertently changed. For example in the expression $λx.xy$, the
variable x cannot be changed to $y$ as it is occurs free and changing
$x$ to $y$ will make it bound. One can avoid such a situation if we
always use fresh variables. We will assume from now on that all
$α$-conversion takes care of this problem.

# $β$-reductions

The computational power of $λ$-calculus comes from what is known as
$β$-reductions which formalises the notion of the $λ$-abstraction. The
essence of $β$-reduction is that the expression $(λx. M)N$, under
$β$-reduction, reduces to $M'$ where $M'$ is the expression $M$ with
all *free* occurrences of $x$ replaced by $N$. However, we have to be
careful to take care that no free variable in $N$ gets inadvertently
bound as a result. For example consider the expression $M = λy.xy$ and
$N=y$. If we blindly reduce $(λx.M) N$ then the free variable $y$ of
$N$ gets bound. This is because a free occurrence of $x$ in $M$ comes
in the scope of a $λy$ and $y$ happens to be free in $N$. To avoid
this problem one can $α$-convert the $M$ to use a new bound variable
instead of $y$. We assume from now on that each such $β$-reduction
carefully avoids the free variable capture.

A *$β$-normal form* is an expression for which there are no more
$β$-reductions possible. For example a variable is a normal form so is
expressions of the kind $λx.x$. One can think of this as a program
that has terminated. We say that an expression $M$ *has* a normal form
if there is normal form $N$ to which it can be reduced after a series
of finitely many $β$-reductions. Expressions might have a normal form
or can diverge and it is *undecidable* to check which is the case.
However, a consequence of the [wikipedia:Church-Rosser theorem]() is
that if an expression has a normal form then it should be unique.

A *reduction strategy* is an algorithm to choose which sub-term of a
lambda calculus expression should be $β$-reduced next. Clearly there
is no strategy that will terminate always as there are terms which do
not have a normal form. Can we have a strategy which will guarantee
termination if the expression has a normal form? Fortunately the
*normal order* evaluation strategy is normalising, i.e. it finds a
$β$-normal form if it exists (See [wikipedia:Lambda Calculus]() for
more details). Therefore, the problem of finding the normal form is
*partially recursive* in the sense that one can write a program to
compute the normal form of an expression if it has one.

# Fixed points and recursion

It might not appear so but hidden inside the simplicity of lambda
calculus is a full fledged programming language. With appropriate
encoding of natural numbers (see for example
[wikipedia:Church encoding]()) one can represent all computable
functions. This is the Church-Turing hypothesis. While we do not want
to go into the details of this, we will show how to implement one
important feature namely recursive definition of functions.

An important property of untyped lambda calculus is that every lambda
calculus function $F$ has a *fixed point*: given $F$, consider the
expression $X = λx. F(xx)$. One can easily show that the term $XX$
$β$-reduces to $F(XX)$ and is therefore the fixed point of
$F$. Furthermore, the computation of fixed point is effective as well,
i.e. we have a lambda calculus combinator for computing it: Consider
for example the combinator (i.e. lambda calculus term with no free
variable) $Y$ defined as $$Y = λf . (λx . f(xx))(λx. f(xx))$$ It is
easy to verify that $YF$ is the fixed point for $F$. Existence of
fixed point combinator is important as that is what allows us to
define functions recursively. A recursive definition $f = F(f)$ is
nothing but the fixed point of the function $F$. The fact that it is
effective make it possible for the compiler to support recursive
functions. Fixed point theorem also allows mutual recursion. For this
one has to have a way of *pairing* values into tuples which can be
done in lambda calculus by suitable encoding (see the
[wikipedia:Lambda calculus#Pairs]()). The reset of the details I leave
it as an exercise.


[lambda calculus book]:
    <http://mathgate.info/cebrown/notes/barendregt.php>
	"The Lambda Calculus: Its syntax and semantics"
