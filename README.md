# Nixy's Server Box

What is: few things that warp didn't provide that I needed for making websites and website servers.

Features:

* Routing
* Markup abstractions
    * Inline functions/markup transforms
    * ```\<head\>``` is a function of ```\<body\>```
    * Variable substitution
* Let's Encrypt's Webroot plugin route
* Easy logger interface
* Easy AWS S3 bucket/DigitalOcean spaces interface

## *Another* Server Library? C'mon.

Yeah, sorry. It *is* minimalist, at least. In fact, that's its premier feature: it's small &amp; simple, built atop common mathematical constructs, using plain-ol' Haskell language rather than a fancy-shmancy eDSL. I want to work close to the server, close to the HTTP implementation itself. This library is just the few functions I needed to write in order to make warp decent for my minimalist website-server needs.

For the same reason that people like *e.g.* Servant or Scotty&mdash;their pretty-print safe RESTful API-friendly syntaxes&mdash;I dislike them. I don't want to be bothered with transformer stacks or special syntaxes. You don't need to take any time to figure-out how to fit a ```Conduit``` ```Consumer``` into the server box; you just stick it in and maybe wrap it with a call to ```pure```.

I think it's good to remember that *HTTP servers are simple already*; when we try to simplify them more, we just add fluff, thus making them more complicated. The server box makes no attempts to accomodate your needs beyond responding to HTTP requests.

It is, as is all my code, not about being easy as much as being general, elegant, composed of common powerful mathematical objects.

So yeah, the server box certainly isn't for everyone. It's much more about staying productive at a low-level, rather than an easy-to-use, pretty framework that fits together like puzzle pieces. In fact, it encourages considering what all an HTTP server *can* be (as long as it conforms to the spec) rather than what it *should* be. Go nuts and live dangerously.

## Not the Best

I'm no webdev master; thus I do things naïvely. This is a double-edged sword: it means that I have the perspective of a newbie, and so I can design newbie-friendly frameworks. Of course, on the other hand, my code isn't as complete, safe, nor optimized as many others'. For example, one of the first things I did in creating a server framework was supporting HTTPS; I turned to *Let's Encrypt*'s *webroot* plugin. I found a blog article showing how to use it in Haskell with warp, and it worked! Yay. What more do I need? It works. The code is just a few lines.

But later I found that snoyberg [already has a Let's Encrypt warp plugin](https://github.com/snoyberg/warp-letsencrypt). I just read the code. I'm sure it's good code, but I don't understand its complexities. I didn't even consider that someone had released code for that, because I didn't think there was that much code to be written for it! (See [my implementation of webroot](https://github.com/nick-chandoke/nixys-server-box/blob/master/src/ServerBox/Plugins/Webroot.hs) for contrast.) Therefore I assume that I must not really know what I'm doing. What I do know is that, as far as I can tell, this framework does everything I want how I want. I figure others may use it and have the same experience.

## Markup Design Concepts

tl;dr: WYSIWYG bad, WYSIWYM (What You See Is What You Mean) good. Prefer TEX over ODT. Use graph editors for graphs, table editors for tables, etc. Let them be converted to HTML automatically only.

### Semantics, not Magazines

One of my general interests as a programmer is finding the most elegant expressions for data definition and manipulation. One example is that nobody wants to write HTML by hand. We have Markdown for blog-like markup, but even in general, we encode information in more specific formats: spreadsheets, graphs, music, equations,...and each has its own editor, because each is a different concept. What concept does HTML represent? None; it's too general. Thus it should never be edited manually &ndash; only produced by some data conversion program, for consumption by browsers. There's no reason to keep an HTML version anywhere but a cache that nobody manually touches.

Secondly, there are only so many common structures: mostly tables, lists, sets, trees, graphs. Maybe it's daring, but I assume that one needs to create only so many general elegant catamorphisms about these few structures in order to create a solid document-to-markup framework that works fine in the grand majority of use cases.

### Templating and Auto-Design

Have you ever used graphviz? The way it works to automatically elegantly arrange graphs is really cool; it uses a concept of potential energy. Anyway, why don't we automatically arrange HTML, too? There aren't many HTML designs: we have list-based things &ndash; navbars, \<ol\>, \<ul\> &ndash;  either vertical or horizontal, hidden or not, on the top, left, right, or bottom of a screen. We have main content, a header and a footer. And of course, some common markdown-supported elements like \<p\>. And of course, we can arbitrarily compose/nest sets or sequences of all these elements. That's the grand majority of webpage content. Why are we needing to layout this stuff by-hand? Let's be honest, folks: we're all basically doing the same webpage design. That's just a testament to the fact that there exists a design that maximizes readability & engagement together. Web design isn't an art project; it can (and *should*) be calculated, given a set of objects and constraints. Semantic web elements, and accessibility, should come built-in, for starters.

**Web design is a science at least as much as it is an art.**

By the way, if you suggest that WYSIWYG HTML editors are good, I ask if you've been to the [CSS Zen Garden](http://csszengarden.com/)?
