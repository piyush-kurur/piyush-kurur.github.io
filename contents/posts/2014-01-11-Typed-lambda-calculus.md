---
title: Typed lambda calculus
tags: Type theory
---

##In memory of [Sanjeev Kumar Aggarwal (ska)][ska].

**Update**: An implementation of propositional logic in agada is
available on my github repository
[sample-code/agda/Logic.agda][sample-code-logic]

In this post, I briefly introduce typed lambda calculus in its
simplest form and explain the type inference rules. The
[wikipedia:Curry-Howard correspndence|Curry-Howard isomorphism]()
appears for the first time here, although only for propositional
logic.


# What are types ?

We fix a set $𝒯$ of base types that can be thought of as the
*built-in* types of the language. For this post, the set of types are
all terms generated inductively as follows:

$$ τ ≔ t ∈ 𝒯  | τ_1 → τ_2.$$

The above inductive definition means that a type is either a basic
type $t$, i.e. an element of the set $𝒯$, or it is the function type
$τ_1 → τ_2$ where $τ_1$ and $τ_2$ are themselves types. The type $τ_1
→ τ_2$ captures the type of functions whose domain is of type $τ_1$
and range is of type $τ_2$. There are few points that we want to
clarify right away.

1. Consider the set of propositional logic formulas where the base set
   of propositions is $𝒯$ and the only logical connective is the
   logical implies $⇒$. There is a one-to-one correspondence with
   types of our language: interpret the functional type symbol $→$ as
   the implication operator $⇒$. We will see in this post that this
   connection is *not* just skin deep.

2. The types in our language are defined independent of the terms in
   our lambda calculus. While this is possible for the version of
   typed $λ$-calculus that we define here, when we want more powerful
   types this will no more be possible. We will have to define the
   values and types together.


# What is a well typed expression ?

Intuitively, a typed lambda calculus is a version of lambda calculus
where each expression is assigned a *type*. This type is used to
ensure that function application is of the correct type, i.e. whenever
an expression $f$ of type $σ$ is applied on another expression $e$ of
type $τ$, the type $σ$ *should be* a function type $τ → τ′$, for type
$τ′$. This requires us to assign types to lambda calculus terms in a
systematic way. We use the notation $e:τ$ to assert that the
expression $e$ has type $τ$. Recall that a $λ$-calculus term is a
variable, or an application or a $λ$-abstraction. Therefore, we need
to give systematic rules for assigning types for these three cases.

The base case would be to assign types to variables in an expression.
We ensure that the $λ$-abstraction also have to assert the type of the
variable it quantifies. Thus, any $λ$-abstraction is of the form $λ
x:τ. e$ and the bound variables $x$ gets its type from this
abstractions, i.e. every free occurrence of $x$ in $e$ is assumed to be
having the type $τ$. For the free variables, the only way we can have
types is by *type assumptions*, a set of assertions of the kind $x :
τ$. The type of an expression thus depends on the type of the free
variables in it.  Therefore, a well typed lambda calculus expression
should be a combination of an expression and a set of type assumption
for the free variables in it. Furthermore, they should satisfy some
additional rules which we call the *type inference rules*.


A *well typed $λ$-calculus expression* is an ordered pair of (1) a
type assumption $Γ$ together with (2) a typed $λ$-calculus term $e:τ$,
written in the logical style $Γ ⊢ e:τ$, subject to the following
inference rules:

Variable Rule (**VAR**)
	: $$ \frac{}{Γ \cup \{ x : τ\} ⊢ x : τ}.$$

Rule for application (**APP**)
	: $$ \frac{Γ ⊢ f : τ_1 → τ_2;\;\; Γ ⊢ e : τ_1}{ Γ ⊢ f e : τ_2}.$$

Rule for $λ$-abstraction (**ABS**)
	: $$ \frac{Γ \cup \{x : τ_1\} ⊢ e : τ_2}{Γ ⊢ (λ x : τ_1 . e) : τ_1
	→ τ_2}.$$


The notation that we used for defining well typed terms is like a set
of logical deduction rules. The stuff on the top of the horizontal
line are *pre-conditions*, i.e. stuff which we already have derived,
and the stuff in the bottom are *consequences*. i.e. stuff that can be
concluded given the pre-conditions on the top. Let us now interpret
each of these rules informally:

**VAR**
	: This rule states that with no pre-conditions we can derive the
	type assertion $x: τ$ provided it is already an assumption. This
	rule takes care of the free variables in the expression.

**APP**
	: This is the rules that makes sure that the functions are applied
	to the right arguments. It makes sure of two things (1) We are
	allowed to form the expression $f e$ under the type assumption $Γ$
	if and only if $f$ and $e$ have types $τ_1 → τ_2$ and $τ_1$
	respectively under the assumption $Γ$ and (2) then the resulting
	expression $f e$ has type $τ_2$ under $Γ$.

**ABS**
	: This rules assign types to the bound variables. This rule needs
	a bit of getting used to as we normally think in the other
	direction, i.e. in the expression $λ x : τ_1 . e$, the occurrence
	of $x$ in $e$ has type $τ_1$.

In all the above rules, we assume that the type assumptions that arise
on the left hand side of the $⊢$ symbol satisfies some obvious
conditions like, no variables should have two distinct type
assumptions etc.

# Curry-Howard isomorphism.

Recall that any type $τ$ can be seen as a proposition over the set of
basic proportion $𝒯$ by interpreting the function type operator $→$ as
the Boolean implies operator $⇒$. Consider the three type inference
rules that we discussed and replace any type assumption $x : σ$ or
type assertion $e : τ$ in the rules above with just the associated
proposition $σ$ and $τ$ respectively. This gives a set of inference
rules for a restricted form of propositional logic that has implies
operator $⇒$ as the only Boolean operator. This isomorphism between
type inference and logical inference rules is often called the
Curry-Howard isomorphism and is one of the main ideas of type
theory. The goal in the rest of the post is to give a different
interpretation of typed lambda calculus so that some magic is removed
out of this isomorphism.

Consider the type assertion $e:τ$. Normally, we think of types as a
subset of allowed values and the assertion $e:τ$ as saying that $e$ is
a value in the set associated with $τ$. There is an alternate
interpretation which makes the logical connection easy to make. Think
of $τ$ as a logical statement, proposition in this case. An assertion
of the form $e: τ$ is thought of as $e$ being the proof of the
statement $τ$. Alternatively, we think of the type $τ$ as the set of
all proofs of the proposition associated with $τ$ in which case our
usual interpretation means that $e:τ$ means that $e$ is a proof of the
proposition $τ$. In this interpretation, a proof of the proposition
$τ_1 → τ_2$ should be treated a method to convert proofs of $τ_1$ to
proofs of $τ_2$. This then gives a valid interpretation of all the
rules of type inference as rules of building proofs.

**VAR**
	: We can create a proof $x$ of $τ$ if we have such a proof by
	one of the assumption.

**APP**
	: If $f$ is a proof of $τ_1 → τ_2$ then it is a function that
	converts proofs of $τ_1$ to $τ_2$. If in addition, we have a proof
	$e$ of $τ_1$, we can apply $f$ to it and get the proof $f e$ of
	$τ_2$.

**ABS**
	: If assuming a proof $x$ of $τ_1$ we were able to get a proof $e$
	(which can make use of $x$ as an axiom) of $τ_2$ then the function
	$λ x: τ_1 . e$ is a proof of $τ_1 → τ_2$ as it takes any input
	proof $p$ of $τ_1$ and produces as output the proof $e[x := p]$ of
	$τ_2$. Here $e[x := p]$ denotes replacing all free occurrence of
	$x$ by $p$.

This essentially is the crux of the "types as proofs" view point. If
we want to effectively use this logical content of type theory, we
need to look at richer and richer types and that is what we would be
doing. We make some modest enriching in this post and leave the rest
for future posts.

## Introduction/Elimination rules.

For the logical operator $→$, the **ABS** rule serves as the
*introduction* rule as its post-condition is an implication. Any proof
of a formula that is essentially an implication therefore would have
**ABS** as its last step. On the other hand **APP** serves the dual
purpose. It is the *elimination* rule for the operator $→$.  You may
think of **VAR** rules as both an introduction as well as an
elimination rule (it introduces nothing and eliminates nothing). This
style of presentation of logical rules is due to Gentzen and plays an
important role in designing type theoretic systems. The introduction
rules gives ways to "construct" objects of a given type and
elimination rules gives ways to "use" the objects in expressions. All
other operators that we introduce here will also have introduction and
elimination rules.

# The operators $∨$ and $∧$.

We would like our logic to have the conjunctions and disjunctions. At
the type level we just need to add two additional type formation
operators $∧$ and $∨$. As a result our types are now inductively
defined as:

$$ τ ≔ t ∈ 𝒯 | τ_1 → τ_2 | τ_1 ∧ τ_2 | τ_1 ∨ τ_2.$$

To prove conjunctions and disjunctions, we need ways to create values
(remember they are our proofs) of type $τ_1 ∧ τ_2$ and $τ_1 ∨ τ_2$
respectively. For conjunctions, we introduce the primitive $(.,.)$
that pairs up two expressions to give a new expression. A value of
type $τ_1 ∨ τ_2$ can be created in two ways: by applying the
*constructor* $\mathbf{inl}$ on a value of type $τ_1$ or by applying
the constructor $\mathbf{inr}$ on a value of type $τ_2$. These give
the required introduction rules for the logical operators $∧$ and
$∨$:

**DISJ**
	: $$ \frac{Γ ⊢ e : τ_1}{Γ ⊢ \mathbf{inl}\; e \;: τ_1 ∨ τ_2},$$
	$$ \frac{Γ ⊢ e : τ_2}{Γ ⊢ \mathbf{inr} \;e \;: τ_1 ∨ τ_2}.$$

**CONJ**
	: $$ \frac{Γ ⊢ e_1 : τ_1;\; Γ ⊢ e_2 : τ_2}
		{Γ ⊢ (e_1,e_2) : τ_1 ∧ τ_2}.$$

The justification of these rules from a proof theoretic point of view
is that one can give a proof of $τ_1 ∧ τ_2$ by giving a *pair* of
proofs where the first component is a proof of $τ_1$ and the second a
proof of $τ_2$. Similarly, we give a proof of $τ_1 ∨ τ_2$ by giving
either a proof of $τ_1$ or $τ_2$ except that we need to explicitly
state which is the case by using the appropriate constructor
$\mathbf{inl}$ or $\mathbf{inr}$.

Next we give the elimination rule for $∧$. The corresponding language
primitive that we need is the projection operators which we add as
built in functions in our calculus.

**PROJ**
	: $$ \frac{Γ ⊢ e : τ_1 ∧ τ_2}{Γ ⊢ \mathbf{fst}\; e \; :\; τ_1},$$ $$
	\frac{Γ ⊢ e : τ_1 ∧ τ_2}{Γ ⊢ \mathbf{snd}\;e \;:\; τ_2}.$$

Similarly, to obtain the elimination rules for $∨$, we add the
function $\mathbf{either}$ that does case by case analysis.

**CASE**
	: $$ \frac{Γ ⊢ e : τ_1 ∨ τ_2;\;Γ ⊢ l : τ_1 → τ;\; Γ ⊢ r : τ_2 → τ}
		{Γ ⊢ \mathbf{either}\;l\;r\;e\;:τ}.$$


We define the the type $σ ↔ τ$ of logical equivalence of $τ$ and $σ$
as $(σ → τ) ∧ (τ → σ)$.

# Truth, falsity and negation.

We have a proof theoretic view of truth and falsity in this setting.
Propositions are "true" if they can be proved by giving a $λ$-calculus
expression of that type and not true otherwise. In that sense, we only
have "truths" and "not truths" and every inhabited types, i.e. types
for which we can construct an element with that type, is
true. Explicit truth and falsity can be achieved by adding types $⊤$
and $⊥$ to the basic types $𝒯$ and to make $⊤$ provable, we enrich the
$λ$-calculus with a single constant $\mathbf{obvious}$ of type
$⊤$. That there is no constants with type $⊥$ is deliberate design
choice as we do not want to prove $⊥$ in our logic. Once we have the
type $⊥$, we can define the negation $¬ τ$ of $τ$ to be the type $τ →
⊥$.

The *law of excluded middle*, LEM for short, is the statement $τ ↔ ¬ ¬
τ$ and is *not* a basic axiom of our logic. We can in fact prove one
direction $τ → ¬ ¬ τ$: Consider the function $λ x f . f x$. It is easy
to see that we can assign to it the type $τ → (τ → σ) → σ$ for any
types $σ$ and $τ$. In particular, if $σ$ is the type $⊥$, we have $τ →
¬¬τ$. The converse however is not provable. This might be considered
as a weakness of the logic because LEM is used through out
mathematics. However, there is a distinct advantage here.

1. The proofs that we build are *constructive* and

2. If we are in dire need of the opium called LEM, we can recover it
   by adding a constant $\mathbf{lem}$ of type $τ ↔ ¬¬ τ$ the way we
   added the constant $\mathbf{obvious}$. However, this is never done
   in practice when using a proof assistant like `coq` or `agda`

# Comparison to untyped $λ$-calculus.

For any typed $λ$-calculus term $Γ ⊢ e : τ$ we can get an untyped
$λ$-calculus term, denoted by $e$ itself, by ``erasing'' out all types
from abstractions. Is it possible to do the other way?  Can we assign
types to an untyped $λ$-calculus expression in a way that is
consistent to the rules defined above? Doing this algorithmically is
the type inference problem. For example, consider the expression $λ x
. x$. Intuition tells us that this can be assigned any type $τ → τ$ as
it is the identity function. Indeed this is the case:

1. $\{x : τ\} ⊢ x : τ$  by **VAR**

2. $⊢ (λ x : τ . x) : τ → τ$ by **ABS** and (1).

It turns out that not all terms can be consistently typed, for example
$λ x . xx$ cannot be assigned any type (why?).

Recall that the computational content of untyped $λ$-calculus is
captured in the notion of $β$-reduction. To avoid variable collision
we also need $α$-conversions. It is easy to see that notion of
$α$-conversion and $β$-reduction can be defined in a straight forward
way for typed $λ$-calculus. What then is the difference?  The typed
version of $β$-reduction is *strongly normalising*. It turns out that
term like $Ω= (λx . xx) (λx .xx)$ and fixed point combinators cannot
be consistently typed. As a result general recursion, and hence
infinite loops, are *not* possible in this calculus.

# Consistency and recursion

The Curry-Howard isomorphism gives us a way to define logical system
out of typed lambda calculus. Enriching the basic typed lambda
calculus with constants like $\mathbf{obvious}$ or $\mathbf{lem}$ is
like adding axioms to the logical system assoicated with the language.
In any logical system, we need to worry about consistency. In
classical logic, a set of axioms together with the inference rules
form an *inconsistent* system if one can prove a statement $τ$ and its
negation $¬τ$. This definition is not very useful in the type
theoretic setting as it is crucially dependent on negation which we
want to avoid as much as possible. An alternate way to define
inconsistency is to define inconsistent system as those which proves
all statements. It is this definition of inconsistency that is easier
to work with in the type theoretic framework. We say that a type
theoretic system, i.e. the under lying typed $λ$-calculus and its type
inference rules, is *inconsistent* if *every* type in
*inhabited*. This makes sense because an inhabitant of a type $τ$ is
the proof of the statement (associated) to $τ$. In this section, we
want to connect consistency and the ability to do recursion. In fact,
arbitrary recursion, or equivalently a uniform way to compute fixed
points, is dangerous from a consistency perspective.

We saw that the typed $λ$-calculus that we defined does not support
fixed point combinators and therefore does not support recursion. This
severely limits the kind of programs that one can write in such a
language. However, we *do* know that fixed points can be implemented
on a computer. Can we enrich the $λ$-calculus to include a fixed point
combinator by force? After all, we do know how to compile it into
machine code. What would happen if we just enrich the calculus with a
constant $\mathbf{fix}$, much like the way we included a constant
$\mathbf{obvious}$ or $\mathbf{lem}$. For this to workout, we would
need to augment our type inference rules with the following rule for
$\mathbf{fix}$

**FIX**
	: $$ \frac{Γ ⊢ f : τ → τ}
		{Γ ⊢ \mathbf{fix}\;f\;:\; τ}.$$

This would mean that, if we some how create a function of type $f:τ→τ$
then we can prove $τ$ using the proof $\mathbf{fix}\; f$. Recall that,
for any type $τ$, the typed lambda calculus expression $I = λ x : τ
. x$ has type $τ → τ$. Taking its fixed point $\mathbf{fix}\; I$ will
give an inhabitant of $τ$. Therefore, adding arbitrary fixed points
will make the logic inconsistent.

Real world programming languages like Haskell does not care about this
issue as writing infinite loops are too important for a programmer. In
fact, every type in Haskell has an inhabitant, namely
`undefined`. What this means is that the type system of Haskell is not
directly suitable as a theorem prover although we can still use it to
catch many bugs at compile time.

Languages like agda which has to double up as a proof assistant allow
certain restricted kinds of recursion by making sure that the
recursion is well formed. Other than the motivation to write real
world programs, some restricted form of recursion is actually
necessary to capture mathematical objects like natural numbers etc. We
leave these issues for future posts.


[ska]: <http://cse.iitk.ac.in/users/ska> "Sanjeev Kumar Aggarwal"
[sample-code-logic]: <https://github.com/piyush-kurur/sample-code/blob/master/agda/Logic.agda>
