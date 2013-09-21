---
title: Functional Programming
active: Teaching
---

# Functional Programming

The aim of this course is to introduce the functional programming
paradigm. Building and maintaining complex software requires a
disciplined and modular approach to problem solving. Modern day
functional programming offers significant advantage over imperative
programming (See ["Why functional programming matters"][whyfp]) when
tackling complex programming tasks.

Any first course has to introduce the theoretical foundations, like
lambda-calculus, type inference that are central to functional
programming. However, I believe that unless there is hands on
experience, much of this knowledge has no impact on the way we create
software and use it. There are two ways in which I have tried to
incorporate these real world aspects.

1. Whenever we deal with a theoretical aspect of functional
   programming, we will also look at actual implementations. For
   example, when we study type inference, we will actually implement a
   type inference algorithm for a toy, but instructive, subset of a
   functional language.

2. A semester long group project that involves building a non-trivial
   piece of software.

We will be using [Haskell] as the programming language for this
course. [Haskell] is the de facto standard when it comes to pure
functional programming language.

## Tentative list of topics to be covered.

* Basic types : integers characters strings lists
* Data structures
* Type classes
* Monads and Input output
* Combinator Parsers
* Generic Programming
* Real world applications

## Some useful links

* [Old homepage of this course](/teaching/cs653/index.html)
* [Some notes that have not been updated yet](/teaching/cs653/notes/index.html)
* [Haskell homepage][haskell]
* Online edition of [Real world Haskell][realworld]
* [Web programming in Haskell][haskellWeb]

## Some applications written in haskell

* [Darcs]  a version control system
* [Xmonad] a window manager
