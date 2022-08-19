---
title: "Untangling Inform7, Part 0 - The Preamble begins here."
date: 2019-02-22T20:53:58-06:00
tags: [inform, haskell, interactive-fiction, development, code]
summary: With the recent open-sourcing of the (fabulous) Inform7 programming language for writing interactive fiction, I’ve fallen back down the rabbit hole of understanding how the pieces fit together...and started writing my own Inform7 compiler. This blogpost series (hopefully) will try to shed some light on how it all works.
---

## Introduction
I’ve spent far too long trying, and never getting very far, to write my own interactive fiction library. [Interactive fiction](https://en.wikipedia.org/wiki/Interactive_fiction) is, to put it simply, a broad umbrella covering various text-based games: what most people probably know as “text adventures”. It certainly covers a lot more (gamebooks ala Fighting Fantasy, choose-your-own-adventures, visual novels, the list goes on) than the games of old where you type in things like `GET LAMP`, but that’s not my thing. My interest is just in the internals of these parser-based (because it parses your commands into game actions) IF libraries. Probably the most well-known of the libraries and tools for writing these sorts of games is [*Inform7*, by Graham Nelson](http://inform7.com).

### What is this Inform7 thing anyway?
I imagine that most of the 5 people who will ever read this post already know what `Inform7` *is*, but just in case I’ll give a very brief overview. `Inform7` is a programming language and library for writing parser-based IF games. It is written in a syntax very similar to a declarative, literate English. For instance, the following snippet taken from the [*Writing with Inform* documentation](http://inform7.com/book/WI_3_1.html) describes some parts of a cave system with items scattered about:

```inform7
“Cave Entrance”
The Cobble Crawl is a room. “You are crawling over cobbles in a low passage. There is a dim light at the east end of the passage.”
A wicker cage is here. “There is a small wicker cage discarded nearby.”
The Debris Room is west of the Crawl. “You are in a debris room filled with stuff washed in from the surface. A low wide passage with cobbles becomes plugged with mud and debris here, but an awkward canyon leads upward and west. A note on the wall says, ‘Magic word XYZZY’.”
The black rod is here. “A three foot black rod with a rusty star on one end lies nearby.”
Above the Debris Room is the Sloping E/W Canyon. West of the Canyon is the Orange River Chamber.
```

This will create 4 rooms (The Cobble Crawl, The Debris Room, Sloping E/W Canyon, and the Orange River Chamber) and 2 things within them (A black rod and a wicker cage). The player can then move between these rooms (“go west”, “east”, “up”) and interact with the objects (“take black rod”, “x cage”).

I personally find this fascinating, and am now onto attempt #10 (or something) to try and write my own version. Imitation is the sincerest form of flattery, as they say.

## The open-sourcing of Inform7
The Inform7 compiler has been threatened to be open-sourced for years; many Narrascope talks described that it was coming “soon”, and that it was taking a while because it was a *huge* undertakin (and it clearly was). And then in April of this year, we finally got it: the literate source for not just Inform7, but a package/build manager (`inbuild`), an intermediate bytecode language (`inter`), the literate programming tool that everything is written in (`inweb`), and more.

[**It’s a lot**](https://ganelson.github.io/inform/index.html). A lot a lot. As described on the `intfiction.org` forum post announcing the open-sourcing:

{{< border "dotted" >}}
> In another sense, of course, the website is an endless morass of self-similar C code, and readers will no more read it cover-to-cover than they would an encyclopaedia. The idea is rather that anyone interested in what Inform is doing, and why it was set up that way, can get both an overview and a detailed presentation of literally all its functions.
- [Graham Nelson](https://intfiction.org/t/inform-7-v10-1-0-is-now-open-source/55674)
{{< /border >}}

It’s multiple hundreds of thousands of lines of C (and not just C, but C with a whole bunch of macros that make it even more impenetrable from a cursory read) and whilst it would be utterly incomprehensible without the huge amount of documentation...understanding it is still pretty damn hard.

There are so many moving parts, each of which is explained in 4 different places; not to mention the levels on top that manage the build process, the bundled standard library quirks (slightly different tree structures, slightly different `printf`, slightly different `for` macros..).

If you, for instance, take the page on parsing new adjectives in the `assertions` module - https://ganelson.github.io/inform/assertions-module/3-nar2.html - we have the `Preform` grammar for adjectival phrases, bringing in wordings and word assemblages (from the `words` module), I haven’t the faintest clue what an `SMF` **is** at this point, the use of `Preform` matching against word ranges, the parse tree..you can probably tell I’m not this far into my reading as I write this post yet!
Which leads to the next part; what I’m actually wanting to achieve by writing down this complete ramble of thought.

## Goals

### ...of the blog

I’m enjoying diving into the Inform7 source code. It’s a wonderful puzzle, piecing together all of the subsystems. This isn’t a bash on the documentation at all - it is incredibly thorough. The amount of moving parts makes it a fairly daunting prospect for people who have an interest; as someone said on Discord, it requires you to keep a dozen different things in the front of your head at the same time.
So that’s what I’m hoping to do with these blogposts: *if I can write about my attempts to reconstruct the Inform compiler, I can share the knowledge I’ve gained and processed to try and help others*.

Maybe I’m aiming too high; maybe I’m coming in with delusions of grandeur that anyone will find my posts any use. I can at least hope that maybe it’ll explain something to someone someday!

I have not yet to work out the level at which I’m aiming this. On one hand, my language of choice (in general, but I think a good match for this project) is Haskell; a pure functional language with something of a reputation for being alien to people who don’t know the language. On the other hand, perhaps I can articulate the whys of the code to a layperson audience well enough for someone with a technical mind to follow? We shall have to see.

### ...of the project
As mentioned before, the entire `inform` project is not only very large, but encompasses much more than just the compiler. Most of this, whilst essential for actually **using** Inform to make and publish works, is fairly rote from a technical standpoint. The parts that are really interesting:

- The Inform7 language parser (from code to AST)
- The `inter` generation
- The `inter` to `zcode` compiler
- The various documentation indices (the Skein and IDE overview pages)

How much of this I’ll get done is to be seen in the coming months, but I’d like to be able to produce a program that, for instance, can build and run a copy of Emily Short’s *Bronze*.

### ...and what it is not

So there are a lot of things this won't be. To name a few:

- A tutorial on how to write your own Inform7 compiler. This seems...not only a Herculean task, but also a fairly pointless exercise.
- A complete copy of the source of the compiler I'm writing. Yes, everything will be FOSS on my github. I fully expect it to be more of a very code-heavy dev diary and I'll jump back and forth between sections as I write more...for instance, part 1 (the lexer, which I'm writing now) is not going to be literally the last time I fiddle with the lexing.
- A guided reading of the Inform7 compiler. Sure, I'll be referencing it extensively and it will follow large parts of the documentation; However, much of it is not in the scope of my interests (the whole build manager) or it describes the *implementation* rather than the *design*. Which brings me to the last point...
- Whatever I'm building won't be a carbon copy of the existing Inform7 compiler. Not only is writing a compiler in C a different kettle of fish to Haskell, just writing a copy wouldn't achieve much. For instance, most of the syntax parsing (after sentence division) happens over several passes through the tree with hardcoded priorities (e.g. "to be" probably means instantiation, rather than assignment of values). Most of the pattern matching is done externally - via defining, writing, and parsing Preform grammar; in Haskell, you can do this in the language itself. Plus, of course, it will be interesting to try something different!

## How to build an Inform7 compiler: a vague overview

[The structure page in the Inform documentation](https://ganelson.github.io/inform/structure.html) has a great diagram showing how the flow of the compiler works, though it is more about the entire pipeline from source to executable. The bulk of the work is probably within the sections titled "INFORM7 Stage 1" and "INFORM7 Stages 2 to 4/5".

- Stage 1 is the first pass of the compiler; it lexes (breaks into tokens) the source code and then groups the tokens into sentences. There is also some rough ordering of a syntax tree for cases such as headings (so it can skip sections if necessary). This is where, in my opinion, the diagram is somewhat misleading. The syntax tree produced by Stage 1 is more a hierarchical collection of sentences than a syntax tree as normally talked about (this happens over several passes in later stages). This is mostly describing the `words` and `syntax` modules; there's some additional info in these (dealing with word ranges, or defining tree structures, for example).
- Stage 2 is an administrative stage; it deals with a plugin architecture, error handling, managing tasks, etc.
-Stages 3 and 4 are what Inform is *known* for; these are where the assertions, the world model, the typechecking, the phrases, the rules, all that jazz is worked on.
- Stages 4 and 5 generate Inter, the intermediate code representation before it gets compiled to whichever format (C, glulx, zmachine, etc).
- Stage 6 adds in the more fundamental parts of the standard rules in Inform6; things such as visibility testing come in here.
- Finally, Stage 7 makes the executable and documentation itself.

The functionality is split over a handful of various services that are used repeatedly; for instance one technique the compiler uses extensively is simply running the compiler on additional sentences that it decides it needs - if you say that "in every room there is a candle", every construction of a room will have the line "In <room> is a candle".


## What next?

who knows?