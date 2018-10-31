# nixys-server-box

## Overview

Currently in active development. The purpose of nixys-server-box is quickly creating site-as-a-service servers built on [warp](https://github.com/yesodweb/wai/tree/master/warp). Let it be known now that I don't really know what I'm doing. For example, I just searched GH for "warp" to make that hyperlink, and found that snoyberg [already has a Let's Encrypt warp plugin](https://github.com/snoyberg/warp-letsencrypt). I just read the code. I'm sure it's good code, but I don't understand its complexities. I'd already written a Let's Encrypt plugin for nixys-server-box, because I didn't even consider that someone had already tackled that problem, because I didn't think there was that much code to be written for it!

Anyway, nixys-server-box has tools for easy routing, working with content delivery networks, and generating and caching dynamic or static markup. I throw around a lot of ideas here; I'm just playing around with translating semantic documents into markup combinatorally, and agumenting or generalizing Markdown and HTML. My goal here is simply a vague "to elegantly, simply, neatly, and safely express ideas as markup, without redundancy." Expect to see many catamorphisms where trees are the inital or terminal objects. Expect a lot of "TODO" statements. Note that functions still *work*, despite the general direction of the library being a bit uncertain.

Currently the library has the following notable features:

* easy routing via [the ```StdWarp``` module](https://github.com/nick-chandoke/nixys-server-box/blob/master/src/StdWarp.hs)
* automatically construct <head> by using [```HeadModT```](https://github.com/nick-chandoke/nixys-server-box/blob/d6c5485d92171ea21798da08dfa00e645da1f1e3/src/Html.hs#L59)
* transforms, macros, and variable substitution (see the following section)
* I guess getting an object from DigitalOcean Spaces is notable? Well, you can do it.

## Transforms, Macros, and Variable Substitution
* A *variable substitution* replaces a variable name by its corresponding value. For instance, ```${today}``` → 10/23/2018.
* A *macro* inserts markup into a markup document, such as inserting a <form> template into a Markdown document. Each macro has its own syntax; see the Haddock documentation for general and specific macro syntax.
* A *transform* is a function that transforms markup. For example, the ```autoSections``` transform function transforms headers into sections in Markdown:

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

Variable substitutions and macros are hand-entered into markup code. Transforms are Haskell functions, and are specified in Haskell code only.
A transform has the same type as a macro; it still converts lines of ```Text``` into ```HtmlT IO ()``` via either ```toHtml``` or ```toHtmlRaw```. However, transforms do not accept arguments, and again, are used in code, not specified in markup files. This being said, as a programmer, you can still partition a ```Text``` into subsections, and transform any of those, then zip it all back up. Due to their all-inclusive nature, transforms have more of a pension for being dangerous than macros do.

## Design Concepts

### Semantics, not Magazines

In general, I'm about working with data, and finding elegant expressions for them. HTML here (markup, in general) is just another form of information. I see no reason why people should write in HTML by hand &ndash; not only because we have Markdown, but because one should store & edit ideas in the structure(s) that suits the data best. For instance, we have spreadsheet editors, graph editors, music editors, equation editors. We store these in .ods, .latex, .graphml,.... There's no reason to keep an HTML version anywhere but a cache that nobody manually touches. Just create translators from those semantic documents into HTML. There are only so many common structures: mostly tables, lists, sets, trees, graphs. Maybe it's daring, but I assume that one needs to create only so many general elegant catamorphisms about these few structures in order to create a solid document-to-markup framework.

### Templating and Auto-Design

There's not a lot of variation in webpages; there's always a ```<nav>```, usually inside a ```<header>```, and a ```<footer>``` with links about the site itself. All sites should be reactive. I'm a big fan of the CSS flexbox model, and I abuse it greatly. I don't know why it's not more popular. Maybe some sites need a little extra, and many use things that are considered nice, but are still unnecessary (e.g. social media plugins, ads, analytics.) This framework doesn't concern itself with such things. nixys-server-box is some cross between combinators and templates. There's built-in limited flexibility, to enforce simplicity and good design practices.

I'm generally always trying to find the "basis" (à la Linear Algebra) elements of everything: the smallest set of elegant objects from which all notable composite objects can be assembled. HTML is usually too general a basis for web development; it's too flexible. There's nothing stipulating that markup has to be written in plaintext or a WYSIWYG editor (which should never be used imho. See [CSS Zen Garden](http://csszengarden.com/).) I'm a big fan of seeing HTML as semantic markup rather than like a mere magazine page or webapp. Sometimes HTML and CSS should be written together; for instance, CSS classes may be automatically chosen to create a "best" layout, given a set of constraints about the elements that need to be present on a webpage, using a method similar to the Glade GTK+ GUI editor or the graphviz graph layout engine. I'm sure many will say that web design is simply an art that only humans can practice, but I say there's no art without science! There are patterns, and a computer can deterministically create a great webpage even better than a person! That's a potential goal for me.

---

As always, if I'm acting a fool and there's something similar & better and I'm just blind to it, please let me know about it!
