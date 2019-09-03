# Nixy's Server Box

Hi I'm Nic C and this is my server framework. Es smol.

Web servers are hardly more than morphisms from HTTP requests to HTTP responses. Nixy's Server Box is created in this vein; it's merely a thin wrapper around [warp](https://www.stackage.org/haddock/lts-14.4/warp-3.2.28/Network-Wai-Handler-Warp.html), plus a routing mechanism. Notably, it does not feature:

* monad transformers
* type families
* special syntaxes/eDSLs
* more than one or two (one's a wrapper around the other) framework-specific data types

It has no promise of fanciness. It's plain, small, fast, and simple. If you know your basic Haskeller's cat theory&mdash;arrows, applicatives, monads, and monoids&mdash;and you're familiar with what an HTTP server is, then there's nothing to learn to get started. Specifically, you'll need to know the [`http-types`](https://www.stackage.org/lts-14.4/package/http-types-0.12.3), `warp` and `wai` packages.

## Features

* Routing
* (Naïve) Let's Encrypt's Webroot plugin route
* Easy logger interface (NCSA Common Log Format)
* Easy AWS S3 bucket / DigitalOcean spaces interface

## Minimalism Philosophy

HTTP servers are alredy simple; if we try to further simplify them, we just end-up complicating things. Thus I'm not a fan of most Haskell web frameworks, with their unique eDSLs and monad transformer stacks that obscure the inner workings of the server and require time to learn, but are supposed to make for "prettier code." A pretty veneer around an ugly mess is not pretty.

Server Box does not assume anything around the server you're using it to implement. Thus the routing mechanism is entirely general: it's just as easy to route by time of day, or by query parameters, as it is to route by HTTP method or relative path. There's no special JSON/REST support; that's your responsibility, and you don't need any help; `aeson` is already very easy to use and capable. Or if you're a forward thinker, you could even use `yaml` instead. If you think this generality too cumbersome and annoying, consider:

* route by method and path: `rte m paths = onMethod (==m) >>> routeByPath paths`
* example routing by method and dynamic paths: `rte m = onMethod (==m) >>^ pathInfo >>^ (\case ["wiki", readMaybe @Int -> articleNum] -> fetchWiki articleNum; _ -> ⋯)`

As these functions are so easily defined, yet are not certain to be used, I see no reason to define them in the `ServerBox` module. If you were authoring the [C2 wiki](http://wiki.c2.com/), then it'd be presumptuous to suggest routing by path, for example, since they route by query parameters.

## Personal Library

Currently I'm the only person who's worked on this library. I am not myself a professional web developer, and I don't work at a company; I work alone (by default, not by choice; I'm open to collab!) Anyway, please note that although I design this library for speed and safety, I have only my own personal needs on which to base the server box's design; expect it to lack some features! I'm not intimate with web standards nor needs of professional web developers or other people involved in the web dev production chain. If you want a feature added, please open an issue or send a PR.

For example, one of the first things I did in creating a server framework was supporting HTTPS; I turned to *Let's Encrypt*'s *webroot* plugin. I found a blog article showing how to use it in Haskell with warp, and it worked! Yay. What more do I need? It works. The code is just a few lines. Later I found that snoyberg [already has a Let's Encrypt warp plugin](https://github.com/snoyberg/warp-letsencrypt). I read the code, and I confidently assume that it's good code, but I don't understand its complexities, because my strategy for learning Let's Encrypt was "let's just get this working as fast as possible." I didn't even consider that someone had released code for webroot, because I didn't think there was that much code to be written for it! (See [my implementation of webroot](https://github.com/nick-chandoke/nixys-server-box/blob/master/src/ServerBox/Plugins/Webroot.hs) for contrast.)

So just be aware: the server box is currently designed to be nothing more than a robust HTTP server: no extra features nor frills!

## Resources

Because the server box is minimalist, if you're looking to use it as a web framework, you'll want to use some more packages. I recommend

* [`data-manip`](https://github.com/nick-chandoke/data-manip) for working with and generating HTML
* <s>[`beam`](https://tathougies.github.io/beam/) for databases</s> wow I couldn't get that thing working. bad. tutorial makes it look good, but actually *using* it is another story!
    * perhaps [persistent](https://www.stackage.org/lts-14.4/package/persistent-2.9.2) + [esqueleto](https://www.stackage.org/lts-14.4/package/esqueleto-3.0.0). see <https://www.yesodweb.com/book-1.6/sql-joins>, too.
