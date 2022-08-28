---
title: "Untangling Inform7, Part 1a - How Inform lexes."
date: 2022-08-20T11:34:00+02:00ds
tags: [inform, interactive-fiction, code]
summary: The first part of the compiler is to turn the source code from a stream of text to a stream of tokens. To some degree, Inform does this in a relatively unique way; whitespace is relevant, and not just for indentation. But before all that, it's best to understand we need to generate a bunch of test data from the existing Inform7 test cases and also setup the shell of the compiler.
---

{{< border "dotted" >}}
## Prerequisites for this post

I think for the most part the answer is still "very little". I probably jump too deep into using compiler terminology here, so a vague understanding of parsing would be useful; namely for grokking the traversal of the token stream and also for the discussion on Preform.

{{< /border >}}

## Introduction

And so it begins for real: documenting my attempt(s) to write an Inform7 compiler. This post will cover the very basic framework of the project but the main part will be the lexer (in `uninformed`), which corresponds to most of the `words` module in Inform7 -- some of the word assemblages and vocabulary structure will be deferred until later. This isn't the first step if you attempt to read the Inform7 documentation cover-to-cover -- it is prefaced by build manager setup and plugin architecture -- but it's the first step of the core compiler, so it's where I'll start to write.

As mentioned in the [last post](untangling-inform-0), my aim is not to produce a carbon copy of the Inform7 compiler. Whilst I'm judging my success on whether I can parse everything that Inform7 parses, the internals are likely to be very different. There was a reason I wanted to write that...but I can't remember.

I am also thinking writing these blogposts in the structure of "here is an explanation of what Inform7 does" followed by "and here is an explanation of how I'm choosing to do it" seems like a neat plan. So let's begin!

*Update from halfway through writing:* there's enough going on in explaining how Inform does it that I think I'm going to divide this post in two, and have a "Part a" post of me trying to explain the Inform compiler and a "Part b" of a dev blog. Or something similar; maybe `Untangling Inform` and `Weaving Inform`, to take it apart and to put it back together?

## A brief explanation of lexing

A lexer is a program (or algorithm, or code) that performs *lexical analysis*. When we read some text (as humans), we don't read letter by letter; we group letters (or punctuation, or diagrams, or whatever) into bundles that fit together semantically. We call these...well, words. We don't gain anything by reading the word "lexer" as an `l`, and then an `e`, and then `x`, and another `e`, and another `r`; we just go "yup, that says `lexer`".

A lexer is the equivalent of doing this, but instead of natural language it's for a programming language. A compiler doesn't need to read the line

```C
int main(int argc, char *argv[]) {
```

as 35 characters; it just needs to read `int`, `main`, `OPEN_PARENTHESIS` and so on. At this stage the compiler has no idea what `{` means, or what the `int` represents. It simply knows that `main`, `(`, and `int` should be 3 separate tokens but in the following code
```C
#define TRUE 1
```

that `#define` is 1 symbol, not 2 (`#` and `define`).

Usually, this is fairly straightforward: as we don't assign any meaning to symbols in the lexing stage, we (rarely) need to deal with complex state like context. Whitespace can be safely ignored, and all we are doing is grouping like-characters together. Very often compilers don't even *have* a separate lexing stage, but rather do it at the same time as parsing (so going directly from streams of characters to a structured collection of tokens).

#### Why is it more complex in Inform?

- I

## The `words` module

Inform's lexer is part of the [`words` service](https://ganelson.github.io/inform/words-module/index.html). A service is pretty much a module of functionality that is used in multiple places, rather than only once as a pipeline step. There's a handful of things in the `words` service:

- **Vocabulary** - We want to track that `Thing` and `thing` are the same word, that the word `5` is actually the value `5`, and that the word `fifth` is an ordinal. This is all in the vocabulary module, along with an implementation of a hashmap for performance. I'll be using some of this, but it will mostly come later when we care more about the semantics of words.

- **Word assemblages** - Both for making up new phrases that may or may not exist in the source, but also for fast comparison: if we know the phrase we are looking for is the 14th through 16th words in the source, we can simply compare their vocabulary entries rather than comparing textual strings each time. Mostly deferred to later along with vocabulary when we deal with `lexicon`.

- **Identifiers** - Making it easier to debug word assemblages and phrases by assigning them short and identifiable reference names. Also deferred until we touch upon `lexicon`.

- **Preform** - Well, this is the elephant in the room.


### To Preform or not to Preform?

[Preform](https://ganelson.github.io/inform/words-module/4-ap.html) is described "a meta-language for writing a simple grammar"; it is very similar to [(Extended) Backus-Naur Form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form), if you know of BNF. I don't think I can do a better job of explaining a meta-syntactic grammar than Graham Nelson does in the Inform7 documentation, so I will not. For those who don't want to click through, though, here is a brief explanation:

- We have `nonterminals` - a term that is possibly a bunch of other terms (for instance, `expression` would be a nonterminal that describes what a mathematical expression looks like). They are called *non*terminals because they can contain other sub-terms; an expression may include sub-expressions, to continue the example.
- And `terminals` - a constant or 'final' term. These are the end result that we actually want to match against. A terminal might be a number (5), a word (this), or a piece of punctuation.

Preform is slightly different in that it assigns a naming to sub-components of the nonterminals, along with a result if the term matches. To copy the example given,

```
<competitor> ::=
        <ordinal-number> runner |    ==> { TRUE, - }
        runner no <cardinal-number>  ==> { FALSE, - }
```

Here we have a nonterminal describing a runner (`<competitor>`) which can be one of two forms: a term that matches `<ordinal-number>` (some other nonterminal) plus the (terminal) string `runner`, or the string `runner no` followed by a term matching the rule for `<cardinal-number>`. If we match the ordinal form, we return the value `TRUE` and if we match the cardinal form, we return `FALSE`. Alternatively, we could return the specific nonterminal matches we had for each of the components.

Preform can do a whole bunch more things (matching entire wildcards), too. It's a very useful way to abstract out pattern matching and be able to avoid writing awful C for extracting substrings from phrases. It also has the future potential to swap out the grammar of Inform7 for localisation (though it is noted that this is very experimental and hasn't been properly supported yet).

**However**, I don't think it's necessary for my project.

- Haskell is far more suited to parsing than C. Pattern matching, view patterns, monadic parsing, ADTs all mean that writing the above Preform matcher in Haskell would be...well, just as readable and flexible as the Preform version (if not more so, as we have type safety instead of `void*`!).
- Futureproofing against localisations is very useful for the Inform project, but not something for this.
- I'd like to try my hand at using an [Earley parser](https://en.wikipedia.org/wiki/Earley_parser) and whilst this would use some kind of grammar definition...I'd write it in Haskell or an embedded DSL.
- It's a lot of work to write yet another parser.

So whilst I'll reference Preform a bunch, I won't be writing (or using) Preform itself in this project.

That's the last bit of the `words` module except the lexer, so on we go!

## How Inform lexes

Whilst a lexer in a typical programming language divides the input stream into symbols or tokens, the Inform lexer divides the stream into *words*. This is primarily a semantic difference rather than a syntactic one; the character `,` is considered a word by Inform, for instance. As such, I will use ‘token’, ‘symbol’, and ‘word’ interchangeably to refer to a single token in the lexical stream. In the context of Inform, *word* does make more sense than symbol or token; something like "is" or "can" does not have a standalone meaning.

Unlike many parts of the documentation, [all of the lexer code is in one place](https://ganelson.github.io/inform/words-module/3-lxr.html), so it's relatively well contained. Some parts like feeds are used elsewhere, as well as amending extra sentences onto the text by calling the lexer manually later in compilation, and there are 3 flags (see [The lexer state](untangling-inform-1/#the-lexer-state)) which are modified from elsewhere.

The lexer is fairly rudimentary; it goes through the text character by character with a window of radius 1:

```
This is some line.

    "[The noun] falls onto [the second noun]."
       ~~~
         ^---------------------- next_cr
        ^----------------------- cr
       ^------------------------ last_cr

lxs_kind_of_word = STRING_KW;
lxs_in_literal_mode = TRUE;
lxs_scanning_text_substitution = TRUE;
lxs_this_word_is_empty_so_far = FALSE;
lxs_most_significant_space_char = '\t';
```

Because the window is so small, the state is very granular. We move the window character by character until we detect the end of a word - from punctuation, a space, or because we have another word coming (such as a speech mark for quoted text). A space is inserted (if needbe) to force a word break, and then we loop again.

The only source location information Inform tracks is the filename and the line number. As the lexer only iterates over individual characters, this is manually updated when encountering newlines (contrast this to my implementation using `megaparsec` later on, where there is very little small-step lexing).

We can skip ahead to section 10, where the `lexer_details` struct is defined. This is **not** the internal state of the lexer as the name may suggest, but an object representing a word that was just read: the text that was read directly, the text after normalising, and a pointer to the vocabulary the word is interpreted as. Being C, the lexer *state* consists of a handful of global variables to track where we have lexed to, what we are currently lexing, and what is to be lexed. A lot of this is due to manual memory management; the “high water mark” described in section 13 is the size of the allocated memory for the text, which may need to be extended (when adding new constructs and sentences that were not originally present). Fortunately, we don’t have to worry ourselves about this (much).

### The lexer state

{{< border "dotted" >}}
The lexer is a finite state machine at heart. Its current state is the collective value of an extensive set of variables, almost all of them flags, but with three exceptions this state is used only within the lexer.
{{< /border >}}

Reference: https://ganelson.github.io/inform/words-module/3-lxr.html#SP16

Of these flags, most of them deal with keeping track of how much whitespace (or the letters) we’ve seen or what we suspect we are reading. The three external flags are not set in this module:

- `lexer_divide_strings_at_text_substitutions` - divide `"the [noun] is"` into `"the "`, `'noun'`, and `" is"`. This is set via calling `Feeds::feed_C_string_expanding_strings` or `Feeds:feed_text_expanding_strings` or `Feeds::feed_text_full` with `expand = TRUE`...but I cannot seem to find precisely where generic text substitutions are parsed this way! The closest I've found is [when defining new "To say ..." rules](https://ganelson.github.io/inform/values-module/4-pi.html#SP3_2_1_3), or hidden in the [Imperative Subtrees when parsing say terms](https://ganelson.github.io/inform/assertions-module/2-is.html#SP3_1). Regardless, it's something which seems to be ignored until we know what to do with the substitution.
- `lexer_allow_I6_escapes` - As it sounds, whether to treat `(- -)` as Inform 6 inclusions or not. This is always true, unless we have a nonstandard punctuation set.
- `lexer_wait_for_dashes` - "is set by the extension-reading machinery, in cases where it wants to get at the documentation text of an extension but does not want to have to fill Inform's memory with the source text of its code."

The most important piece of the state is tracking if we are in *literal mode* (and when to enter/leave it). Inform has 4 kinds of words: `ORDINARY_KW` (regular text), `COMMENT_KW` (...comments), `STRING_KW` (quoted text in double quotes), and `I6_KW` (literal Inform6 code). The latter three of these are called literals, because they are considered a single word -- with the exception of text substitutions if the divide flag is on -- by the lexer. Thus, most of the work of the lexer is correctly identifying when to enter or leave literal mode, and then to quickly scan through text otherwise.

### Literal mode

[Section 27.6 - Entering and leaving literal mode](https://ganelson.github.io/inform/words-module/3-lxr.html#SP27_6) deals with entering and exiting literal mode. Importantly, the check for **entering literal mode happens after reading a character in normal mode**. By the time this routine sees if we should enter literal mode, our word already has a `[` or whatever. It checks if we have a comment start (`[`), a string start (`"`), or the second half of an Inform6 inclusion (`-`) when we immediately just read a `(` (and because this is awkward with punctuation, the lexer erases the previous word consisting of `(` and changes it to `(-`). If we're in a comment, we have a generic comment nesting count (because we can't do things like paren-matching in a context-free grammar).

Leaving literal mode is basically the same idea; it happens **before we add the character to a word**, and we check if we are **about to** leave literal mode by reading some punctuation. If we leave literal mode this way, we set the current character to a space. **This is why Inform can only track source location info at a line level**: whenever we need to force a word break, Inform amends the text with a space character. This happens when:

- We leave literal mode (so we've just finished a quoted string, a comment, or an I6 inclusion),
- We read a punctuation mark that isn't a `.` separator in a number or some other cases (and we add spaces around both sides of the mark),
- We are choosing to divide strings at text substitutions, where [the old string is ended and a new string is started](https://ganelson.github.io/inform/words-module/3-lxr.html#SP27_6),
- Or we are at the end of a file, so we amend a bunch of extra whitespace to [make sure we have a clean break between extension files](https://ganelson.github.io/inform/words-module/3-lxr.html#SP25_1).

### The main Inform lexer loop


## Generating test cases

## Interlude: Setting up the project

Apparently I don't have anything for this yet.

## Begin the lexing

### Whitespace

### Punctuation

## Making sure it's all correct

Well, this is where the illusion of a blog post as a living document of how the code evolved is revealed to *actually* be a guy behind the curtain pulling the strings. If it wasn't obvious to begin with, the testing was done in tandem with adding additional cases to the lexer; I sketched out the basic lexing, ran the test cases, and had 200 passing. I found the common fault (for instance, newlines not correctly resetting preceding spacing) and fixed it. Repeat ad infinitum (or ad testinum, I guess). This isn't meant to be a philosophical thought about test driven development or how to write technical blog posts, but...I digress. It felt like describing the end product was a more cohesive piece of writing than a train of thought.

