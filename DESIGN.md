# Design Experimentation

From the very beginning, the purpose of this library has been defining routes elegantly using standard Haskell types, i.e. not needing to define our own complicated syntax/eDSL, nor transformer stacks nor type families, custom types, etc., and *if* I resorted to any of these, I'd minimize them. Ideally the module'd export (or define) merely one newtype.

I've tried a few different designs, and I'm still trying to find the most elegant structure for describing web routes. For that, I'll need to know all the common things that routes need to do. In other words, I'm writing this library according to my needs as they arise when programming servers.

## Before v0.2: `ExceptT`

`newtype Route m = Route { rte :: Request -> ExceptT (Maybe Response) m Response }`. Yes, it's a transformer stack, but at least it's only one layer deep, and it's a transformer that everyone knows. The idea was that one could return `Left Nothing` to signal trying the next response; `Left (Just response)` to short computation and return an response describing some error; and `pure response` for usual "successful" responses.

In the earliest commits, this somewhat odd definition allowed using filter routes as guards, e.g.

    Route $ \req -> do
        onMethod (==methodGet) req
        onPath (last >>> (=='/')) req
        -- normal route here

I disliked needing to pass the request object to each of the filter functions, so I changed them to accept other "normal routes" as parameters, e.g.

    onPath (last >>> (=='/')) . onMethod (==methodGet) . Route $ \req -> -- normal route here

Additionally, being monad-based, this design led to ugly nestings of `pure`/`return` and newtype wrapper constructors at the end of every route.

Eventually I decided that I dislike monad transformers in general. Whenever possible, I'd like to avoid them; they encourage many nested types, each layer of the transformer may have different `Applicative`, `Alternative`, or `Monad` instances that I don't want; and they stack into hugely-nested types which are cumbersome for both the programmer and compiler. A new design was wanted.

## v0.2: Arrows

Starting with v0.2, I'm using arrows:

    newtype RouteArr m b c = RouteArr (b -> Compose m (RouteFlag c))
    data RouteFlag a
        = Next
        | Short Response
        | Route a
        implements (Functor, Applicative, Alternative, Monad)

(`Compose` is there to make the Arrow and ArrowChoice definitions easier. Currently that's the only benefit I can yet see.)

This seems like the best idea, since arrows compose easily, allowing filters to be `RouteArr m Request Request` so that we can do, e.g. `onMethod (==methodGet) >>> onPath (last >>> (=='/'))` without concerning effects or anything smacking of monadic state. This fortunate accident relies on the fact that we must put all the filters first, and all the filters act on the request object. We need neither to pass the `Request` object to filters, nor routes to filter routes. The implementation definitions, as well as `routeToApp`, are easy, short, and neat. There's no hacky `Semigroup` instancing (see [commit a4503f800e65f42bd3b73706e489e999d76e541c](https://github.com/nick-chandoke/nixys-server-box/commit/a4503f800e65f42bd3b73706e489e999d76e541c) and prior.) Overall, defining routes by arrows just looks cleaner, more regular, and more straightforward.

Even so, I thought I'd try using `newtype RouteT m b = RouteT (ReaderT Request m (RouteFlag b))` just in case routes really were better described by monads rather than arrows. Well, the type class implementations for such a type weren't as elegant as those for the arrow type classes, so I nix'd the idea. Also I don't like needing `transformers` as a dependency.
