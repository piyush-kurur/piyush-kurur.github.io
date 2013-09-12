---
title: Webpage Reloaded
tags: Web
---

This is my very first post. It also coincides with the entire
rewriting of my homepage and these two events are not independent.  I
used to use a set of Makefiles for dependency checks, [pandoc] for
generating html, m4 for templating and no css. The stuff worked but it
soon became difficult to maintain.

It was more or less clear to me from the start that I wanted a static
site managed via [darcs], written in [markdown]. Of course there is
[jekyll] but I always thought [pandoc] was way more powerful than some
of the other markdown processors that comes with [jekyll]. And then I
heard of [hakyll]. It uses [pandoc] as its markdown processor which
means that I get all the [pandoc] goodies: easy math integration,
syntax highlighting, and possibility of using different input (say
latex) and output (say pdf) formats. Besides, it is written in
[my favorite programming language][haskell]. No more excuses for a bad
homepage.

I *never* thought my page would ever be styled with css. As a language
(if you can call it one) css is pretty lousy, maybe slightly better
than html. Besides, no two browser seems to agree on the
standard. Who, in their right senses would want to work with it ?
[Compass] made me change my opinion. Firstly, you can use the [sass]
now instead of css, an advantage comparable to using [markdown]
instead of html. Secondly, it has mixins that take care of all (most)
of those browser incompatibilities. It might not go well with IE
users: I don't know, neither do I care to know. But it should work
mostly. The style for this page is entirely written in sass using the
[compass] framework.  I will publish the source code soon after some
refactoring.

Thanks to the great softwares mentioned above, I now have a clean
homepage complete with a blog and atom/rss feeds. Some lecture notes
that I had are not yet hakyllised. It will soon be.

A big *thank you* to the folks behind [hakyll], [pandoc] and
[compass].

**Update 18 May 2013**: I have added my old wrtings as blog post. So
it might appear as if this not my first post.

[jekyll]: <http://jekyllrb.com> "Jekyll"
