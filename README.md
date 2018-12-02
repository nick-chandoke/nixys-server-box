# nixys-server-box

## Overview

**Purpose:** be a template/framework for quickly & easily defining safe, fast, flexible, custom SaaS HTTP servers built on a small set of common functions. It's aimed at anyone who wants to quickly throw-together a server with custom routing and application logic without worrying about details. nixys-server-box documentation is verbose enough to make it both tutorial and reference, as an introduction to common web things, so that one can get started quickly, hacking and learning when they wish. It's not the best that a server can be, but it should good enough for many. It's aimed at common use cases, namely serving markup from a CDN, constrained to some application logic.

## Not the Best

I have no problem admitting that I began learning Haskell, basic server design, and HTTP, in February 2017, so I'm myself a newbie. Thus I do things naïvely. This is a double-edged sword: it means that I have the perspective of a newbie, and so I can design newbie-friendly frameworks. Of course, on the other hand, my code isn't as complete, safe, nor optimized as many others'. For example, one of the first things I did in creating a server framework was supporting HTTPS; I turned to *Let's Encrypt*'s *webroot* plugin. I found a blog article showing how to use it in Haskell with warp, and it worked! Yay. What more do I need? It works. The code is just a few lines.

But later I found that snoyberg [already has a Let's Encrypt warp plugin](https://github.com/snoyberg/warp-letsencrypt). I just read the code. I'm sure it's good code, but I don't understand its complexities. I didn't even consider that someone had already tackled that problem, because I didn't think there was that much code to be written for it! Therefore I assume that I don't really know what I'm doing. What I do know is that, as far as I can tell, this framework does everything I want how I want. I figure others may use it and have the same experience.

### An Unfinished Playground

Aside from being a generally reliable framework, this is also an idea testing ground. My goal here is simply "to elegantly, simply, neatly, succintly, and safely express ideas as markup." Some such ideas are:

#### Unimplemented &ndash; Just Considering

* translating semantic documents (*e.g.* spreadsheets) into markup combinatorally

#### Implemented

* agumenting or generalizing Markdown and HTML, *e.g.* via use of macros
* binding \<head\> imports with HTML (*e.g.* putting at least one ```CodeBox``` in your markup guarantees that exactly one [prism](https://prismjs.com/) import is in \<head\>)
* macros and variable substitution (see the following section)

##### Macros & Variable Substitution

* A *variable substitution* replaces a variable name by its corresponding value. For instance, ```${today}``` → ```10/23/2018```
* A *macro* inserts markup into a markup document, such as inserting a <form> template into a Markdown document. Each macro has its own syntax; see the Haddock documentation for general and specific macro syntax.

<pre><code>
    ## Heading
    some text
    ### Subheading
    more text

    ### Subheading 2
    ⋯
</code></pre>

becomes

    <section>
        ## Heading
        some text

        <section>
            ### Subheading
            more text
        </section>

        <section>
            ### Subheading 2
            ⋯
        </section>
    </section>

which would later be converted from Markdown into HTML.

Variable substitutions and macros are specified in markup, not in Haskell code.

**NB**. Macro names given in the haddocks (/e.g./ @@AutoSections)are just examples; you must specify the actual name in calls to the ```Map``` argument of ```runMacros```.

## Design Concepts

### Semantics, not Magazines

One of my general interests as a programmer is finding the most elegant expressions for data definition and manipulation. One example is that nobody wants to write HTML by hand. We have Markdown for blog-like markup, but even in general, we encode information in more specific formats: spreadsheets, graphs, music, equations,...and each has its own editor, because each is a different concept. What concept does HTML represent? None; it's too general. Thus it should never be edited manually &ndash; only produced by some data conversion program, for consumption by browsers. There's no reason to keep an HTML version anywhere but a cache that nobody manually touches.

Secondly, there are only so many common structures: mostly tables, lists, sets, trees, graphs. Maybe it's daring, but I assume that one needs to create only so many general elegant catamorphisms about these few structures in order to create a solid document-to-markup framework that works fine in the grand majority of use cases.

### Templating and Auto-Design

Have you ever used graphviz? The way it works to automatically elegantly arrange graphs is really cool; it uses a concept of potential energy. Anyway, why don't we automatically arrange HTML, too? There aren't many HTML designs: we have list-based things &ndash; navbars, \<ol\>, \<ul\> &ndash;  either vertical or horizontal, hidden or not, on the top, left, right, or bottom of a screen. We have main content, a header and a footer. And of course, some common markdown-supported elements like \<p\>. And of course, we can arbitrarily compose/nest sets or sequences of all these elements. That's the grand majority of webpage content. Why are we needing to layout this stuff by-hand? Let's be honest, folks: we're all basically doing the same webpage design. That's just a testament to the fact that there exists a design that maximizes readability & engagement together. Web design isn't an art project; it can (and *should*) be calculated, given a set of objects and constraints. Semantic web elements, and accessibility, should come built-in, for starters.

**tl;dr** web design is a science at least as much as it is an art.

By the way, if you suggest that WYSIWYG HTML editors are good, I ask if you've been to the [CSS Zen Garden](http://csszengarden.com/)?

---

As always, if you know of a better method or see a bug, please open an issue on GitHub, or email me!

I understand that one would want HTML functions separate from HTTP ones; if I add one more HTML/markup-munging function (in addition to the already exant ```deriveTOC``` and ```lineCode```,) then I'll split those off into their own markup-manip library.
