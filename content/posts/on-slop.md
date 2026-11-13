# I tried vibe coding for 3 weeks and it kind of sucked: On vibe coding, slop, craft, and enjoyment.

So yeah. I ended up vibing up a project where I only cared about the end result and it made me realised how much it sucks. Then I spent all evening writing down a huge ramble.

---

I was going to write a post about my experience trying out vibe coding and how it kind of sucked, the result kind of sucked, and a play-by-play of how everything slowly got sucked into the slop mines. On the bike ride home, whilst my mind raced over all the things I could say, I realised: "hang on, I'm just making human slop about how slop kind of sucks." and the first thing I did when I got home was delete it all. Goodbye, 2500 words. I'll elaborate a little in the Slop section, but first I want to touch the big stuff: vibe coding (or LLM coding, or AI coding, or whatever you want to call it).

I will start with the disclaimers: I did not like AI. I still do not like AI. I strongly disagree with any notion that begins with a premise of "and it's here to stay, so-". The only reason I ended up trying this was because open-weight models, as I heard in snippets here and there, had reached a point of being actually coherent. I almost exclusively used DeepSeek V4.1 Flash, running on providers that stated no training or data retention. In total I ended up using about $25 of API credits ($15 of this was for actual development, $5 on me having failed to setup the pi-blackhole context/compaction/memory extension correctly and it deciding to use kimi-k3 to summarise the conversation at a cost of something like 15x the model I was using to produce the code, and then $5 scraping and collecting information on various spinoff series) . A rough estimate of energy usage I pencilled out came to about 8kWh, or running an oven for about 4 hours. This is not a "therefore, this isn't harmful!" disclaimer, of course - but hopefully it is clear that I am *not* writing this as a retrospective justification, or a both-sidesism.

I did not cross, and will refuse to cross, my hard lines: no money to alleged rapist Sam Altman, to arguably culpable for negligent manslaughter Anthropic, or to white supremacist owned xAI; no generative artwork, video, or media; at most it should cost me a couple of dollars a day if I use it heavily.

## On vibe coding

### The project

I started a hobby project which for the first time I can think of, I was interested in the end product rather than the enjoyment of coding. I wanted to make a site that would catalogue the episodes of the Great British Bake Off and allow you to track/tick off challenges as you baked them yourself. I like seeing number go up, I like ticking stuff off as progress towards a completion, and I really like baking.

I was only really interested in the end product because it was - and is - a fairly unexciting software project. It's a CRUD app. You just need to join together a database, a server program with a list of API endpoints, and then display that stuff in a pretty way on the frontend.

It was not intended to be a vibe coded mess, but it did end up that way. I am a backend developer (with no real frontend experience), so I was completely fine and in my zone when it came to writing the first draft of the backend. I used some libraries that were new to me for a bit of variety compared to my day job. It was enjoyable, but then I started getting bored of the boilerplate. I know whatI'm doing, I'll own every line and manually adjust it to my weird preferred formatting and layout. Bam.

Then I got to the frontend. Okay, this is a fun learning experience. Oh, now I've got the data I need to make it look even slightly decent. **My mistake: eh, I don't care to learn CSS. I'll read these blogposts about CUBE and stuff and then just ask the LLM to do it.** (I still don't care to learn CSS, just for what it's worth. Even if the first thing to do when I finish writing this is to go write some CSS myself.)

Now I have a draft site that runs and it looks pretty good, and I don't need to read the CSS or the HTML templating that got generated for me. I'll do that later. Oh, I do think the background is the wrong colour. Give me some prototypes of different layouts. Maybe we need some more real content. No, I think it looks too much like I just vibed out a website, fix that. Let's ignore the cognitive dissonance that I did just vibe out a website.

This is where the original blogpost was intending to be an after action report detailing exactly what I was writing and what I was offloading to the LLM and why. It quickly became "everything". My best laid plans to understand and own the code lay in pieces. I didn't let it commit to git, at least, so the history was full of +15000 line diffs that say "commit some more stuff" in proper developer fashion.

I was not going full hands-off vibe. I was browsing over the code every few prompts, making manual adjustments if something felt really ugly. I was asking the LLM to go refactor this or that or to move these 500 lines to another file. At the start this was really thorough where I would be editing almost every line, but quickly that stopped in favour of "go do another refactoring pass. I don't care about the diff looking nice".

At the end of the 3 weeks or so, I was quite pleased with it. It looked quite pretty - apparently all you need to do is spend 10 minutes prompting "please make it look less like LLM slop" and it does that just fine, which says *a lot*. I had spent about $25 and a solid $10 of that was just from using the LLM as a fuzzy web scraper to seed the catalog of episode data. No, I don't really want to be trying to find information on what the contestants made for the showstopper challenge in the 5th episode of the 4th series of the Czech spinoff of GBBO and then spend 10 minutes turning it into JSON.

![](/images/01-landing.png)
![](/images/04-episode.png)
![](/images/14-bakes.png)

### It's done...or is it?

It was alive! I was running out of features that I actually wanted to use in the end product: There was a ton of catalog information about different challenges, I had a whole bunch of links to recipes from the show, I could plan a bake or upload a photo of what I had made to my account and track it on the community page. I was happy. I didn't care about the software underneath it, but that was fine to me - I just need to tweak some CSS or edit some text to make it sound less bad.

So then I fired up the site to do one last round of tweaks and bug fixes. I tallied 83 individual items that annoyed me in some way over the course of 50 minutes. I prompted the LLM, with a skill I made which forced it to put things into a text file so it wouldn't forget about them after it lost the context, to:

- please identify what needs to be done for each item,
- please do some sort of seriousness or urgency tracking,
- ask me for input if there's anything that is a design decision,
- not actually implement things.

You're absolutely right—I should do all those things. Let me investigate. I went off and tidied the kitchen for 10 minutes, which is really quite a nice thing to do now that xkcd's "Code's Compiling!" comic is sadly no longer relevant. I came back and it was done with my housekeeping: Now I had a 750+ line TODO file with checkboxes, every line I had written of the form "the button looks weird" turned into a mini product pitch about how the button's aesthetics were unsatisfactory and demanded a new change. Alright, I'll skim the list. Yup, it has covered everything. Now go start on implementing the list. No, I don't really care what you do first. Just do stuff. See if you can spin up some agents. Off I go, back to tidying the kitchen.

**But...is this it? Is this really what I want to be doing? What purpose is any of this serving?**


About 10 minutes later, I came back and it had fixed about 4 or 5 of them. I sat down and fired up some of the code. Oh god. What the hell is *any of this*. That was the final straw for me. I could've just let it continue going and fix the other 78 bugs, but I was so *done* with it all that I just stopped it there.

- I don't know frontend development, so I expect that making html elements and layouts is going to be verbose...but I don't think "literally zero code or element reuse" is the answer there.
- What does any of this even *do*?
- Why is every comment 5 lines long and sounding like a product pitch?
- I know CSS is long-winded, but does this site really need almost **9000 lines of CSS**??

And so on, and so on. It just got worse the more I looked. I asked the LLM and even it went for "yup, we have 850 css classes and only 25 of them are ever used more than once. you have 14 different mastheads. what are you even doing".

So now, where are we:

- I have a working site that looks mostly pretty alright and functions pretty alright at its current destination
- ...that nobody can understand
- that nobody *wants* to understand
- that has very little written with intent, purpose, or reason
- that has a list of 83 problems
- ...and those 83 are just the ones we've not previously plastered over.

### I'm going to start again, but that's not an option for real projects.

I'll cut the rambling short here and offer the one and only solution: **this has shown me that indeed, my hobby project idea is a cool idea! Now I actually have to make the damn project from scratch.**

This is not a general, feasible, preferable, or many other words to indicate a slight positive, solution. It works here because the app I made is:

- Small, boring, and simple. Seriously. It's just a server that reads from a database and then puts it on a javascript webpage.
- Not actually being used for something, by someone. The one user (me) had used it so far to store one thing (a photo and a description of a lovely persimmon cake I baked).
- Inconsequential. There is no client, or deadline, or monetary involvement, or product. It really does not matter at all except to be a kind of prettier spreadsheet where I can tick off that I made croissants that week.
- Something I actually know about. Again, I may not be a *frontend* developer but I'm skilled enough in software engineering and programming and coding to be able to pick up enough to get by.

The moment you have something that cannot just be thrown aside as a "that's cool, let's move on" is when you cannot just "do it again". You need to maintain it. You need to keep it going. You need to grit your teeth and work on it.

You keep going; or rather - dug your way into this pit with LLMs - you keep digging with more LLMs. It doesn't matter if you've got a little bit of code duplication - the LLM will just update all of those sites when it makes a change. It doesn't matter if every item on the page is its own CSS class - more elements? more classes. Simple.

You are stuck. Your software continues to *work*: it appears to *function*, as long as people stick to the parts you've prompted for. It appears to be *maintainable*: if someone moves away from the Potemkin village and reports a breakage, a fix is simply a "bug report: the button on this page disappears when..." away. It can continue like this for a long time, but it is not maintained by you, nor by the LLM. It simply appears to be functioning as if it is maintained.

### "Ah but you prompted it badly! You used a crappy model! You didn't specify it properly!"

## On slop

This is where my preamble about my second epiphany comes into play. Amusingly, the thought of writing this blogpost has given me more thought about software architecture than 3 weeks and a billion tokens of actually producing software.

So okay, setting the scene. I had some thoughts shortly after it clicked into place that I was *done* with my "vibe experiment" and I wanted to go back to coding *properly* and making *software that feels like it was made for a reason*. These were along the lines of "aha! I should write up a big retrospective over the last 3 weeks. I speak to a lot of friends and acquaintances who, like me before this, were fervently against the terrible vibe coded mess the world is. I can explain all about how the community sucks, and how the code sucks, and how it was fine at this point and-". I was enthused. Then it came to actually putting words on a page. Ugh, this sucks. Okay, I'll write about what the Bakeoff is and that I used this technology and...8 paragraphs in. **Why? Why am I doing this?**

- Am I enjoying writing it? No.
- Am I growing or learning writing it? No. I'm literally just recounting events of me making crappy software.
- Am I going to get use out of it? No, see above.
- Am I feeling enthusiastic that others will enjoy, grow, or learn from it? ...no, not really.
- Am I thinking others are actually going to give a damn?
- ...so what on earth is the point of doing any of this?

**Ironically, my epic tale about how slop sucks was just human slop.** I wasn't producing it because I got anything from it. I wasn't producing it because I wanted others to get something from it. I was just producing it for the sake of producing. That, my friends, is slop.

### Producing for the sake of producing

I really like [this article about the rise of "Whatever"](https://eev.ee/blog/2025/07/03/the-rise-of-whatever/). The fact that stuff gets made because Something is better than Nothing. That seems to be the purpose and the reason for vibe coding. I have not found a better example that completely refutes the idea that vibe coding is "democratizing software" or whatever else people want to call it. Can anyone, without any particular set of skills, now produce? Yes! Why are they producing? Almost always: Because they can!

So now the "moat" - the thing that was acting as a barrier or an obstacle - of writing code is gone. No longer do you need to understand how to write that function or how to produce that GUI with that library to have an app that does what you want. You don't need the massive time investment to think over design decisions, over writing thousands and tens of thousands of lines of code. You don't need to sit there banging your head when your character in your game floats through the wall. For everyone who *only* cared about the end result - which is (I hate this phrase) absolutely valid and completely fine - they can get there much more easily.

But that's not what is happening. Before, people would write their own little programming languages for fun. It was a fun activity. Aha, turns out a compiler is very easy to vibe code. Now you don't need to learn about parser generators, or IR, or LLVM, or optimisations when you can just ask the LLM to make it for you. But...now what? okay, you have your CoolLanguage:

- Did you enjoy writing it? Almost certainly not - you wanted the end result, and you got your end result. Unless you got a weird pleasure from writing "Hello please add a while keyword".
- Did you grow or learn from it? Definitely not.
- Am I going to get use out of it? Well no. Anything your language can do, there's going to be a language that has been developed by thousands of people for decades that has every feature you want with 100 libraries and integrations for any aspect of development you could want. You *could* make a game in it if you write some OpenGL or Vulkan bindings, or you *could* make a networking library...but those are just to get the satisfaction of writing them yourself.
- Are others going to want to interact with it? Definitely not.
- So...what purpose does it serve?

You made something just to say you made something. You did not make it with a purpose, or an audience, or an intent. You did not gain anything from making it and nobody else is gaining anything from making it. All you have done is put more **stuff** into a world already overloaded with **stuff**.

### Why collaborate or improve when you can just produce?

This is something I experienced early on in my "journey". I was setting up my development environment. I settled for the `pi` coding harness, which is honestly pretty good. It's designed to be super minimal (it connects to your choice of LLM, and by default it has read/write and the ability to use `bash`) compared to something like Claude Code, and then you install plugins and extensions as you want them.

Okay, let's fire up the subreddit and see what people like to use. "What's your favorite subagent extension?" - ah, this seems useful. "Just make your own" - I mean I don't want to do that but ok. "I made my own - look nthere. It's got all the features I want" - Yup, that is a github repo with endless amounts of badges, logos, slogans, text formatting. It has a big feature bulletpointed list, complete with excessive numbers of emojis, about how it's reliable and featureful and effective. It has installation instructions for 3 different node package managers, for homebrew on mac, for arch linux, to build from source, from curl, for freebsd, for the Amiga. It has 300 commits over the span of 6 days and it has a big image of the number of GitHub stars it gained over time. The total number of stars is 5.

That's great if you like to have a well-organised repository. It's great that you've got a README that contains all the information someone might need...but *nobody does need any of it.*

Then you keep going: what about these various different kinds of compaction plugin which claim to have better memory ability? Yup, this is software alright - everyone saying that the one they use is great and the others are bad with no justification. This used to be just fun internet slapfights, but now for every slapfight there's 10 more posts.

I just released `super-cool-agentic-compaction`—lightning fast, reliable, extensible, fantastic whatever the hell. I'm so excited to announce the release that I have *asked Claude to write up a description of it*. The description doesn't actually tell you anything, but it makes sure to use lots of emojis and em-dashes. I'm so enthused about presenting this library that I don't want to spend even 3 minutes going over the text I am posting to make it sound *vaguely* human. Why have I made this? Well it solved a problem for me. That problem was that I was finding it inefficient to solve more problems.

### Even if you only care for the destination, you need a reason to make the journey

There was a great twitter post from someone heavy on the AI train about how he had unlimited access to some frontier model and he was showing off the 300,000 lines of Rust libraries his highly orchestrated and organised swarm of agents had produced by themselves in some 4 or 5 days. It was a bunch of network protocol distributed systems stuff. Everything is super reliable and detailed and featured - the top reply, with far more likes, from yet another AI techbro: "is any of it actually useful?"

The phrase "it's not about the destination, it's about the journey" gets said a lot in empassioned pieces against AI. It's not just about the *end product*, but it's about all the things you do and experience to get there. I do strongly agree. I am someone who cares greatly about the journey and for the most part this has been my reason for making projects. What we are seeing in this wave of slop, however, is people being given a car and them going for a drive not because they enjoy driving or because they need to get to their destination. No, they just need to get *somewhere*. I'm glad I got to my destination super fast rather than spending 3 weeks hiking. Did I have a reason to come here? No, not really. Did I enjoy my way here? Nah. Does being here actually mean anything, for me, or for others?

This is not a general broad net thrown over every piece of vibe coded software out there. I have indeed seen some apps and programs made for a purpose, where there is no journey to be savoured. They function as intended, or appear to. They fulfill their intent. Often that's something fun - hell, that was *my original intent with the Bakealong project*. In many of the cases I'm thinking about here (I will not mention them for the sake of privacy) it's filling an actual important need that improves someone's life and I cannot - I *will not* - criticise or put that down negatively. If AI must be a tool, then it must be a tool **used with intent**.

## On craft

### Programming, development, and software *is* a craft and an art - and that matters
This is where it gets a little bit uh, fuzzy. Possibly the most hotly debated topic that comes up, time after time. If you can ignore the environmental, ethical, moral, imperialist, whatever else (you can ignore the environmental issues?) issues with the AI industry as it stands - is programming an art form? Is the craft of software something that matters beyond the enjoyment and satisfaction of the programmer? If we can have LLMs that do not sacrifice quality, that do not pollute the groundwater, that do not require people in the global south to spend hours a day classifying horrific images of child abuse for pennies - isn't the "craft of software" just how much you enjoy your job?

So I'll save you the ramble: Yes, the craft of software development is important. No, it cannot or should not be dismissed as whether or not a programmer enjoys their job. No, it does not just mean the objective technical quality of a line of code.

Ok well I'll give the ramble anyway. The idea that writing lines of code is a means to an end is a fallacious one. It is correct that yes, it does not matter how those lines have ended up in a file to be compiled. The *intent* and the *decisions* behind those lines, the way we have ended up at that position, are what matters. That is something that is lost in the LLM process.

When we develop software, we are (at least trying) to specify how a program works. Code is, after all, the specification medium that converts human intention (the what) into machine instruction (the how). It is sufficiently detailed enough to be understood by a computer, but sufficiently human enough to be read and understood by a developer. All LLMs have done in this regard is shifted the specification several cranks to the left - now the specification (the prompt) is done in human language rather than in a coding language, which makes it easier to specify for a human but much harder to reliably be interpreted by a computer. But that's something else.

We, as software developers, know this and plan for this. We write design documents and technical requirements and specifications and we have endless meetings about what the implementation will look like. We discuss and we plan, we architect and we scaffold and we refine and we *craft*. In an ideal world, nobody would be coding anything without an absolutely clear picture of what needs to be done and how to do it - but that is simply not the case, ever. You write code and you refine it and you plan it and you *craft* it as you go. You can do this because you have *intent*. You have *purpose*. You can explain why you did that thing there when your colleague asks about it in the code review and even if it turns out to be the wrong decision - there was a reason for it. There's a new reason that was clearly missed, but there's still a reason.

Yes, 90% of the work is not exciting. 90% of it is going to be connecting this microservice to that database with a data pipeline that does a third thing, and that's not exactly fun. That is why I think solely talkng about the craft as a measure of how fulfilled the programmer feels in their work is cutting it short. That 90% is being done in the way we choose *for a reason*.

### No craft means no vision means no reliability

Maybe a bit of a tenuous connection but it works for an idea I've been yelling around. Okay, so let's continue to think that craft is just about enjoyment. We can use "spec-driven development" or whatever to ensure that the LLM knows what needs to be made. As long as we are firm (hah) and consistent (hah) about having our requirements defined up-front, we don't need the craft. What you can't - or don't - put into a specification is the *vision*. You don't think about the software in 5 years. You don't need to know or care about if it works in 5 years.

But, increasingly, **who cares**? Your startup idea about scalable agentic solutions for data pipelining isn't thinking about 2031. You're thinking about having enough of a working product to get the venture capital round and bring in 5 more people, then to get acquired by Google. Who cares if the product works in 5 years?

You see this in the recent surge of "reverse engineered" software. In quotes, because it's not actual reverse engineering - it's LLM-directed copying. I'm thinking about the free versions of Photoshop, or Illustrator, or whatever else. The idea that now because anyone can go type "claude, make me photoshop but free. make no mistakes" we have solved software. Now, I detest Adobe as a company and would be among the many celebrating if they collapsed - but *there is a reason people pay thousands and thousands of dollars for their software*. It doesn't just work for this one thing; it works for *everything*. You are never going to hit the point that you want to do something but Photoshop doesn't support it, because they have had hundreds of engineers designing this thing for 30 years. It doesn't really matter if the code behind Photoshop is a giant mess that is painful for someone to understand or maintain. It's messy but it *works for everything*. This is compared to the vibe coded mess I mention earlier - where it's messy but it only *works for a little*.

**Vibing software does not produce software that stays around**.

## On enjoyment

This is probably the least important part for the corporate world reading this and probably the most obvious thing for the hobbyists reading this. If you enjoy making *anything*, or taking pride in your work, or *learning*, or *improving*, or a hundred other things - you are not being productive. You are choosing not to produce at maximum production efficiency. *Oh of course* that's fine they say. *Of course* some people like writing code by hand. But it's not something that should be done in the *real world* anymore.

**Fuck that.**

Software, technology, industry, the real world is not apolitical. It's not objective, sterile, cold, or optimised. The overwhelming data points to AI mandates being miserable. Ignore the quality problems, ignore the reliability problems, ignore the maintainability problems - and you still see, time after time, **nobody taking pride or care in their work**. Developers who have devoted their life to the craft are now being reduced to rubber stampers or reviewers. They exist to, to quote some article I can't find the link for, tell Claude to go ahead and continue for 8 hours a day...so now, when everything collapses, who is going to be around to fix it?

So yes, ensuring enjoyment and satisfaction in coding is less efficient in the amount of stuff produced - but it is invaluable in keeping *productivity*.

## In conclusion

Well I got home, wrote this for 2 1/2 hours straight, forgot to eat dinner, and now wondering where the hell I was going with this. I am glad I reflected on myself. I am glad I caught myself before I joined the waves of people producing for the sake of producing. I am glad I caught myself before convincing myself that the ends justify the means. I still feel I have not put my thoughts to paper on a hundred other things:

- Programming is not just a craft but an *art*. The file is my canvas and the lines are my brush and I can claim my code does not look sterile and cold but expressive and vibrant. Yes, I'm joking around a little but I do stand by this.
- Using LLMs as a "tool" to assist in human coding does not completely fix the problems. They still remain - it eats away at the craft, it hides the intent, it produces for the sake of producing, it cuts the quality. You can say you'll go back and refactor that structure but let's be honest - *will* you?
- I have not discussed the actual **cost** of AI infrastructure at all in this post. Hoooooo boy.
- When (*when*, not *if*) this all goes away, what are we going to be left with? Is the software world just going to be forever a hellscape?
- It's just all so damn *tiring*.

I'm tired.
