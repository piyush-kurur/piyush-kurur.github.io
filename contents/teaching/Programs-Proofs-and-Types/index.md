---
title: Programs, proofs and types.
active: Teaching
---

# Programs, proofs and types

Software is becoming large and complex. More and more, control of
critical infrastructure like railway signalling are being done in
software where software bugs can have disastrous consequence. Such
systems cannot be left to the mercy of ad-hoc programming. This course
is about engineering reliable software. We study the technique of
building _certified software_, i.e. software that comes with _formal
proofs_ of correctness.

The system that we use in this course is the [Coq] proof assistant.  A
proof assistant is a language which helps in constructing mathematical
proofs whose correctness is checked by the computer (machine
checked). The underlying theory of a proof assistant like [coq] is
quite general and is closely related to programming: one can see
building formal proofs as building programs. Thus a natural
application of proof assistants like [coq] is in building certified
software. However, these systems are often very powerful and are
powerful enough to formalise and prove virtually any mathematical
theorem --- for a recent success see the complete proof of
[Feit-Thompson theorem in coq][ftcoq].

# Who should take this course?

The focus of this course is certified programming. Nevertheless, it
should also be of interest to mathematicians and logicians who want to
know how a proof assistant like [Coq] work and what they are the good
for.

# Pre-requisites.

[Coq] and other type theory based systems can be seen as natural
generalisation of functional programming languages like Haskell and
ML. Thus a familiarity with these languages will definitely help in
the course. However, I have not put it as an official pre-requisite.

This course should be considered as a fairly advanced course and hence
plenty of mathematical maturity is expected. You should also be
comfortable installing [coq] and associated software. Latest GNU/Linux
distribution comes with pre-packaged versions of [coq] that should be
sufficient for our purposes.

## System requirements

You should have an up to date installation of [Coq] and related
software.  The easiest way to get such a system is to install for
yourself a basic GNU/Linux distribution like Debian or Ubuntu. [Coq]
comes pre-packaged for these distributions.

When developing serious code in [Coq] you would need to use the proof
general mode in emacs. [Coq] comes with an ide for building proof
scripts which I have found quite painful to use.

# External links

* [Course repository][ppt] on bitbucket. This repository contains all the
  course related material including the course proposal. We also
  encourage you to use the wiki and the issue tracker that is
  available with this repository.

* [Certified programming using dependent types][cpdt]

* [Software Foundations][sf]

[cpdt]: <http://adam.chlipala.net/cpdt/>  "Certified Programming using Dependent types"
[sf]: <https://www.cis.upenn.edu/~bcpierce/sf/current/index.html> "Software foundations"
[coq]: <https://coq.inria.fr/> "The coq proof assistant"
[ppt]: <https://bitbucket.org/piyush-kurur/ppt> "Course repository for programs proofs and types"
[ftcoq]: <http://www.msr-inria.fr/news/feit-thomson-proved-in-coq/> "Feit-Thompson theorem in Coq"
