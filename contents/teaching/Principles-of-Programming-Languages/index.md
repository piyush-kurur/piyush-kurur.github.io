---
title: Principles of Programming languages.
active: Teaching
---
# Principles of Programming languages

The course will loosely follow the text book
["Concepts Techniques and Models of Computer Programming"][textbook]
by Peter Van Roy and Seif Haridi. The primary platform used in this
book is the Oz language. One of the advantage of oz is that it
provides features for all the different styles of
programming. However, this is also its weakness. Therefore we will
also look at candidate language for each of the paradigm to better
appreciate the power/weakness of each of these paradigm.

## Assignment submission

* [Assignment submission procedure](./submission)
* [Assignment 1][assignment1] - Due on the midnight of 30 Sep 2013
  (Monday).
* [Assignment 2][assignment2] - Due on the midnight of 4th Nov 2014

## Note:

The Oz package for 64-bit is buggy. If you have trouble installing it
for use you may try the following
[image file of about 2.3 GB (*internal link*)][image-file] created by
[Satvik] with virtual box. The `root` user password is `toor` and that
of the user `debian` is `reverse`. I have not tested it though.

## Course TA

1. Satvik C (email id `satvikc`)
2. Smriti J (email id `smritij`)

## Programming Environment

1. [Oz/Mozart][mozart].

2. [swi-prolog] for Prolog

3. [Erlang]

4. [GHC] for [Haskell]

5. [Agda] for [dependent typed programming][dt]

## Online reading material

1. [Github repository][sample-code] of some sample code that I showed
in the class

2. [Implementation of Functional Programming Language][funimplement]
   Topics covered in the class include:

    i. How pattern matching is compiled efficiently.


3. A complete unification algorithm is given in my
   [functional programming lecture notes][fp-notes]. This unification
   algorithm is tailored for type inference but you can have a look.


## Some difficult (long term) exercises that you can try out.

1. Implement the general [unification algorithm]  described in the
   class. Use any of your favorite programming language although doing
   it in C or Java might not be such a good idea.

   Beware: many algorithms described in literature have subtle bugs.

2. Code up the resolution algorithm used by Prolog
   ([SLD Resolution][sld]). Implement a simple prolog interpreter
   based on it.

[sample-code]: <https://github.com/piyush-kurur/sample-code/>
[sld]: <http://en.wikipedia.org/wiki/SLD_resolution> "SLD Resolution"

[fp-notes]: </teaching/cs653/notes/lecture-at-a-time.html>

[unification algorithm]:
       <http://en.wikipedia.org/wiki/Unification_(computer_science)>
       "SLD Resolution"

[erlang]: <http://erlang.org> "Erlang"
[funimplement]:
    <http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/>
	"Implementation of Functional Programming Language"
[mozart]:     <http://www.mozart-oz.org/>  "The Mozart Programming System"
[swi-prolog]: <http://www.swi-prolog.org/> "SWI-prolog home"
[agda]: <http://wiki.portal.chalmers.se/agda/pmwiki.php> "Agda Homepage/Wiki"
[dt]: <https://en.wikipedia.org/wiki/Dependent_type> "Dependent types Wikipedia"
[textbook]: <http://www.info.ucl.ac.be/~pvr/book.html>
[image-file]: <http://ppk.cse.iitk.ac.in/ppk/cs350/virtual-image.7z> "Linux Image file"
[satvik]: <http://cse.iitk.ac.in/users/satvikc> "Satvik's homepage"

[assignment1]: <https://github.com/piyush-kurur/sample-code/blob/master/assignments/delhi-metro/delhi-metro.pl>
      "Assignment 1: Delhi Metro"

[assignment2]: <https://github.com/piyush-kurur/sample-code/blob/assignment/assignments/traffic/controller.erl>
