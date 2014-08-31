---
title: Dependent types
tags: Type theory
prev: /posts/2014-01-11-Typed-lambda-calculus.html
---

In the [last post] we looked at simply typed lambda calculus. The
logic that we were able to capture was propositional logic. What do we
need in our language to get predicate logic. Recall that we think of
types as mathematical statements and elements of that type as proofs
of the statements. If we were to capture propositions $P$ on say the
natural numbers $ℕ$ in our type theory, we need a type $P(n)$ for
every natural number $n$. A *dependent type* is such a mapping from
values in a type $a:A$ (in this case the type $ℕ$) to types $P(a)$.

Dependent types can be captured formally by first postulating a "type"
for all types which we denote by $\mathrm{Type}$. A *dependent type*
or a *type family* $P$ *on* a type $A$ is nothing but a function $P: A
→ \mathrm{Type}$. For each give element $a:A$ there is a type $P(a)$
and constructing an element $y : P(a)$ should be thought of as proving
$P(a)$.

Given a set $A$ and a predicate $P$, which in the type theory world
becomes a type $A$ and a type family $P: A → \mathrm{Type}$, we need
types that capture the logical formulae $∃(x∈A) P(x)$ and $∀(x∈A)
P(x)$. This is done through the dependent sum ($Σ$) and the dependent
product ($Π$) types respectively.

# $Σ$-types.

For a type $A$ and a type family $P : A → \mathrm{Type}$, the
*dependent sum type* (or $Σ$-type) $Σ_{a:A} P(a)$ consists of all
pairs $(a,x)$ where $a:A$ and $x
: P(a)$. The motivation for this definition is that proving the
statement $∃(x∈A) P(a)$ for sets $A$ and predicate $P$ on $A$
*constructively* involves constructing a witness element $a:A$ for
which $P(a)$ is true. What more, in the constructive setting, $P(a)$
being true has to be demonstrated by a proof $x:P(a)$. Thus elements
of $Σ_{a:A}P(a)$ can be thought of as proofs of $∃(x∈A) P(a)$. Clearly
if $Σ_{a:A}P(a)$ is not inhabited, then $∃(x∈A) P(a)$ is not provable.

The ordinary product type $A × B$ (or $A ∧ B$ in the [last post]) can
be seen as the $Σ$-type $\Sigma_{a:A} P$ where $P$ is the constant
function $λ x → B$ that maps every element $x:A$ to the unique type
$B$. We can also define the ordinary sum type $A + B$ (or $A ∨ B$ in
the [last post]) as the $Σ$-type $Σ_{x:𝔹} P$, $𝔹$ is the boolean type
containing two values $\mathrm{True}$ and $\mathrm{False}$ and $P$ is
the function $λ x . if x then A else B$.

The first component of any element $z$ of $Σ_{a:A} P(a)$ gives an
element of $A$. For types $A$ that do not have any inhabitants,
therefore it is impossible to construct element in $Σ_{a:A}
P(a)$. This is in line with the idea that $∃(x: ∅) P(x)$ is false.

# $Π$-types.

The *dependenent product* (or the $Π$-type) $Π_{a : A} P(a)$ consists
of functions $f$ whose value $f(a)$ is of type $P(a)$. The motivation
from the logical side is that a proposition $∀(x∈A)P(x)$ can be proved
by giving a function $f$ that takes every element $a:A$ to a proof
$f(a) : P(a)$. In agda the type $Π_{a : A} P(a)$ is denoted by $(a :
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

We now give a sketch of the type inference rules for a dependently
typed lambda calculus. Since values can occur in the definition of
types, the type inference rules gets complicated in two ways. Firstly,
our strategy of defining types independently of values that we used in
the [previous post][last post] will not work; we need to define them
hand in hand. Secondly the order in which we make type assumptions
starts mattering. As a result set of type assumption $Γ$ are replaced
by *telescopes* or *ordered* sequences $⟨x_1:A_1,…,x_n:A_n⟩$ where
each type $A_{i+1}$ is allowed to refer to only variables
$x_1,\ldots,x_i$ in it and not $x_{i+1},\ldots, x_n$.


[last post]: </posts/2014-01-11-Typed-lambda-calculus.html>
	"Typed lambda calculus"
