---
title: Dependent types
tags: Type theory
prev: /posts/2014-01-11-Typed-lambda-calculus.html
---

In the [last post], we looked at simply typed lambda calculus where
each term was associated with a type. As such the types of the
language and terms were completely separated --- we could define the
types of the language independent of terms. A dependently typed
language is one where the types can depend on values from a given
type. For example consider the natural numbers $ℕ$. In a language like
Haskell, we can define them inductively. However, what if we want to
capture the collection of residue classes $ℕ_n$ modulo a number
$n:ℕ$. Ideally, we would want to distinguish the types $ℕ_m$ and $ℕ_n$
for each $m ≠ n$ --- the term $x
+ y$ should lead to a type error if $x:ℕ_m$ and $y:ℕ_n$. A dependently
typed language can supports construction of such types. For a type
$A$, a *type family* $P$ on $A$ is a collection of types $P(a)$ one
for each $a:A$.

In the [last post], we saw that the type inference rules of simply
typed lambda calculus gave us the rules of natural deduction for (a
fragment of) propositional logic via the Curry-Howard isomorphism. To
extend this to predicate logic, we would need a dependently typed
lambda calculus as we explain now. Recall that we think of types as
mathematical statements and elements of that type as proofs of the
statements. Therefore, to capture predicates $P$ on a type $A$, we
need a type $P(a)$ for every $a:A$, i.e. we need a *type family* on A.
As before, to prove the statement $P(a)$ should mean constructing $y :
P(a)$.

Given a set $A$ and a predicate $P$, which in the type theory world
becomes a type $A$ and a type family $P$, we need types that capture
the logical formulae $∃(x∈A) P(x)$ and $∀(x∈A) P(x)$. This is done
through the dependent sum ($Σ$) and the dependent product ($Π$) types
respectively.

# $Σ$-types.

For a type $A$ and a type family $P$, the *dependent sum type* (or
$Σ$-type) $Σ_{a:A} P(a)$ consists of all pairs $(a,x)$ where $a:A$ and
$x
: P(a)$. The motivation is that proving the statement $∃(x∈A) P(a)$
for the set $A$ and predicate $P$ on $A$ *constructively* involves
constructing a witness element $a:A$ for which $P(a)$ is true. What
more, in the constructive setting, $P(a)$ being true has to be
demonstrated by a proof $x:P(a)$. Thus elements of $Σ_{a:A}P(a)$ can
be thought of as proofs of $∃(x∈A) P(a)$. Clearly if $Σ_{a:A}P(a)$ is
not inhabited, then $∃(x∈A) P(a)$ is not provable.

The ordinary product type $A × B$ (or $A ∧ B$ in the [last post]) can
be seen as the $Σ$-type $\Sigma_{a:A} P$ where $P$ is the type family
that assigns $B$ to every $a$ in $A$. We can also define the ordinary
sum type $A + B$ (or $A ∨ B$ in the [last post]) as the $Σ$-type
$Σ_{x:𝔹} P$, $𝔹$ is the boolean type containing two values
$\mathrm{True}$ and $\mathrm{False}$ and $P$ is the type family $λ x
. \mathbf{if}\; x\; \mathbf{then}\;A\;\mathbf{else}\;B$.

The first component of any element $z$ of $Σ_{a:A} P(a)$ gives an
element of $A$. For types $A$ that do not have any inhabitants, it is
impossible to construct element in $Σ_{a:A} P(a)$. This is in line
with the idea that $∃(x∈∅) P(x)$ is false.

# $Π$-types.

The *dependenent product* (or the $Π$-type) $Π_{a : A} P(a)$ consists
of functions $f$ whose value $f(a)$ is of type $P(a)$. The motivation
from the logical side is that a proposition $∀(x∈A)P(x)$ can be proved
by giving a function $f$ that takes every element $a:A$ to a proof
$f(a) : P(a)$. In agda, the type $Π_{a : A} P(a)$ is denoted by $(a :
A) → P(a)$.

Dependent type languages gives ways to construct functions in
$Π_{a:A}P(a)$ for any empty type $A$. For example, if $P : ⊥ →
\mathrm{Type}$ is a type family we can create the function f in agda
by using what is known as the *absurd pattern* (see line 2 below).

~~~ {.haskell .numberLines}
proof : (x : ⊥) → P(x) -- this is the dependent Π-type.
proof ()               -- proof is defined using absurd pattern
~~~

This is in line with the idea that $∀(x:∅) P(x)$ is true.

# The `Vector` type: An example from programming.

We now give an example of the famous vector type in agda used to
captures lists of a particular length.

~~~ {.haskell .numberLines}
data Vector A : Type : ℕ → Type where
	[]    : Vector A 0
	_::_  : {n : ℕ} → (a : A) → Vector A n → Vector A (succ n)
~~~

Having defined such a type we can define the function `head`
as follows

~~~ {.haskell .numberLines}
head : {A : Type} → Vector A (succ n) → A
head (x :: _) = x
~~~

One of the advantages of this definition of `head` is that it can
never be applied to an empty vector. This is because the input type
`Vector A (succ n)` of `head` can never match with the type of the
empty vector `Vector A 0` and hence will lead to a compile time error.

# Type inference rules.

We now give a sketch of the type inference rules for dependently typed
lambda calculus. As before typing any term requires to "know" the type
of all the free variables in it. We capture this by judgements of the
kind $Γ ⊢ e:A$. However, unlike in the simply typed case, $A$ might
itself have variables embedded in it. So not just the free variables
in $e$ but all free variables that occur in $A$ should appear in the
type assumption. Thus our strategy of defining types independent of
terms like in the [previous post] will not work; we need to define
them hand in hand.

The type assumptions themselves should now be an ordered sequence $Γ$
of assumptions of the form $x:A$ with the added restriction that for
any assumption $x:A$ in the sequence $Γ$, all the variables that
appear free in the type $A$ should themselves be defined in previous
assumptions of $Γ$. Therefore the order of the assumptions in $Γ$
matters and they cannot be treated as mere sets. Such sequences will
be called *telescopes* of type assumptions. Finally, we can form
expressions like $λ x: A → e$ only if $A$ is known to be a valid type
which in turn depends on the telescope that is effective at that
context. The inference rules for dependently typed lambda calculus
thus needs to define *simultaneously* what the valid types, terms,
telescopes and type judgements are. This we capture via the following
judgements.

1. $e\;\mathbf{term}$ which asserts that $e$ is a valid term,

2. $Γ\;\mathbf{telescope}$ which asserts that $\Gamma$ is a valid
   telescope,

3. $A\;\mathbf{type}$ which asserts that $A$ is a valid type and
   finally

4. $e:A$ which asserts that the term $e$ is of type $A$.


We use $ε$ to denote the empty telescope. Any judgement $Γ \vdash
\mathbf{something}$ means that the judgement $\mathbf{something}$ is
valid under the telescope $Γ$.

We write $ε \vdash \mathbf{something}$ as just $\vdash
\mathbf{something}$ or even $\mathbf{something}$. To reduce a lot of
boiler plate, we sometimes drop certain preconditions if it can be
deduced from the other preconditions. For example, we drop the
precondition $Γ\;\mathbf{telescope}$ if we also have a precondition $Γ
⊢ \mathbf{something}$ latter on.



<!--
  Besides these we have the special type $𝒰$ which we called the
  universe that stands of type of all types. Strictly speaking having
  such a type can make the logic inconsistent (Girard's paradox) but
  for simplicity we assume this.

  $$\frac{}{Γ ⊢ 𝒰\;\mathbf{type}}$$

  In all the above rules $Γ$ is any valid telescope (we have not
  defied what they are yet) and plays no important roles in the above
  rules. In fact, we can ignore $Γ$ and we have essentially just
  defined the types that we had in the previous. Dependent types will
  use these telescopes in a subtle way.

-->

**Rules for telescopes**
: The first rule that we have is that an empty sequence is a valid
  telescope.

  $$\frac{}{ε\;\mathbf{telescope}}$$

  The next inference rule says that we can add an assumption $x:A$ at
  the end of a valid telescope $Γ$ provided it is possible to infer
  that $A$ is a type from the telescope $Γ$.

  $$\frac{\Gamma\;\mathbf{telescope};\; \Gamma \vdash
  A\;\mathbf{type}}{\Gamma, x:A\;\mathbf{telescope}} x ∉ Γ$$

  We have slightly abused the notation in the expression of the side
  conditions $x ∉ Γ$ which essentially says that $x$ is *fresh*,
  i.e. does not occur in any assumptions of $Γ$.


**Formation rules for terms**
: This essentially describes the syntax of our language.

$$\frac{}{x\;\mathbf{term}},$$

$$\frac{e_1\;\mathbf{term};\;e_2\;\mathbf{term}}
	{e_1e_2\;\mathbf{term}},$$

$$\frac{Γ\;\mathbf{telescope};\; Γ ⊢ A\;\mathbf{type}}{Γ ⊢ λ x : A →
e\;\mathbf{term}}$$

The first two are essentially expressing the lambda calculus syntax
for variables and application in the form of rules of inference. The
last rule, however says that the expression $λ x : A → e$ is a valid
term only if it is in the context of a telescope $Γ$ where $A$ is a
type.

**Formation rules for $Π$-types**
: We have the following rules by which we can form types

$$\frac{Γ ⊢ A\;\mathbf{type};\;Γ, x:A \vdash
		B\;\mathbf{type}} {Γ ⊢ Π_{x:A} B\;\mathbf{type}}$$

Notice that we omitted the precondition $Γ\;\mathbf{telescope}$ and
$Γ,x : A \;\mathbf{telescope}$ as mentioned before.

**Rules for type inferences**
: We have a single variable rule followed by the
introduction/elimination rules for the $\Pi$-type.

$$\frac{Γ, x : A\;\mathbf{telescope}}{Γ,x : A ⊢ x : A}$$


$$\frac{Γ ⊢ e_1 : Π_{x : A} B\; Γ ⊢ e_2 : A}{Γ ⊢ e_1 e_2 :
B[x/e_2]}$$


$$\frac{Γ, x : A ⊢ e : B}{Γ ⊢ (λ x : A → e) \; : Π_{x : A} B}$$


To incorporate the rules for $Σ$-types, we need to introduce a
dependent pairing primitive and the corresponding pairing operations.
We leave this as an exercise.

<!--

$$\frac{Γ ⊢ A\;\mathbf{type};\;Γ, x:A \vdash
		B\;\mathbf{type}} {Γ ⊢ Σ_{x:A} B\;\mathbf{type}}$$
-->

# Where are the dependent types?

The dependently typed language we introduced here is mostly useless in
the absence of any interesting types and type families. One strategy
would be to introduce each type and type family by hand giving its,
formation, introduction and elimination rules. At the very least, we
fix a base set of types $𝒯$ and add the formation rule
$$\frac{}{t\;\mathbf{type}} t ∈ 𝒯.$$ This will give us all the types
described in the [previous post] (once we identify $\Pi_{x:A}B$ with
$A → B$ whenever $x$ is not free in $B$). To make dependently typed
systems more useful actual systems supports construction of user
defined types and type families. What these constructions are and what
should be the restrictions on them has to be worked out. We defer this
topic for the future.

[previous post]: </posts/2014-01-11-Typed-lambda-calculus.html>
	"Typed lambda calculus"

[last post]: </posts/2014-01-11-Typed-lambda-calculus.html>
	"Typed lambda calculus"
