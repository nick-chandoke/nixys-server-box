{-# language OverloadedStrings, NamedFieldPuns, DataKinds, GADTs, KindSignatures, FlexibleInstances, TypeFamilies, TypeOperators  #-}
{-# language NoOverloadedLists #-}

-- TODO: make function to AMPlify code (boilerplate, that is)
-- | Semantic HTML for webpages. Rather than be very general for HTML, this Html module encapsulates common HTML data patterns, and creates data types for them, such as radio or checkboxes, lists, etc. Makes markup less verbose, but still structured (as opposed to Markdown, which is non-hierarchical and isn't easily extended.)
--
-- === Nota Bene
--
-- * this library uses /strict/ 'StateT'!
-- * throughout the library you'll see the identified @conv@(ersion function); this refers to either @toHtml@ or @toHtmlRaw@.
-- * functions strive for endomorphismism (endomorpismness?); converting between 'XML.Document' and 'Html' is expensive. @Document@ is suited for general manipulation, while @Html@ is suited for composition only
module ServerBox.Markup
( -- * HeadModT
  Element(..)
, Head
, HeadModT(..)
, HeadMod
, toDoc
, putHead
, mergeHead
, ph
  -- * Macros
, MacroT
, Macro
, runMacros
  -- * Utilities
, varsub
, liftRender
  -- * HTML Endomorphisms
, lineCode
, deriveTOC
  -- * Extra Tags
, headscript_
) where

-- cmark-gfm
-- import CMarkGFM (commonmarkToHtml, extTable, extAutolink)

-- base
import Control.Applicative
import Control.Arrow (second, (>>>), (&&&))
import Control.Monad ((<=<), (>=>))
import Data.Char (isSpace, toLower, ord)
import Data.Ix (inRange)
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Prelude hiding (Applicative(..))
import Data.Function (fix)
import Data.String
import Control.Exception (SomeException)
import Data.Bool (bool)
import Data.Char (isSpace, isDigit, toLower, ord)
import Data.Foldable (toList, foldr', fold)
import Data.Ix (inRange)
import Data.List (intercalate)
import qualified Data.Text as T'
import qualified Data.Tree as Tree

-- hxt
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

-- lucid
import Lucid
import Lucid.Base

-- containers
import Data.Sequence (Seq)
import Data.Tree (Tree(..), unfoldTree)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- miscellaneous packages
import qualified Data.Text as T' -- text
import Data.Text.Lazy (toStrict)
import NicLib.Tree (readIndentedGeneral)

-- | A /macro/ is a function that a user can specify in markup files. Each macro has its own syntax. Macros transform, modify, or remove markup. Look in these haddocks' Contents section to see macros bundled with the server box.
type MacroT h m a =
    (a -> HtmlT m ()) -- ^ conv
    -> [T'.Text] -- ^ text to process
    -> HeadModT h m () -- ^ a macro may modify the Head

type Macro h a = MacroT h Identity a

deriving instance Ord Attribute

-- | Abstract representation of an Html element that belongs in <head>. Usually I expect you'll use @Element Text@; however, I'm leaving it polymorphic so that you can use any @ToHtml a => Element a@, as that's the most general form of element that works with @toDoc@.
--
-- @Text@ should work fine considering that, as far as I know, at least, there are no children of <head> that have any more children than merely a single text node.
--
-- The purpose of @Element@ is to be put in @Head@, which is a set; you'll be using 'Set.fromList' (explicitly, or implicitly via @-XOverloadedLists@) to input that set, which means that the argument type to @Element@ must be ordered. Thus, even though it isn't present in the definition of @Element@, you'll practically need to use @Ord a => Element a@ rather than mere @Element a@. For example, the following would not compile wihout the explicit type:
--
-- @
-- thing :: (Monad m, Ord h) => 'HeadModT' h m ()
-- thing = ph (div_ mempty)
--       *> 'mergeHead' ([ Element "link"   Nothing [href_ $ "some.css", rel_ "stylesheet"]
--                      , Element "script" Nothing [src_  $ "some.js"]
--                      ] :: Ord h => Set (Element h))
-- @
--
-- Well, that, /and/ both @mergeHead@ and 'putHead' require @Ord@ constraints as well! Thus there's a need for @Ord h@ in @thing@'s type signature.
data Element a = Element
    { name :: T'.Text
    , mcontent :: Maybe a
    , attrs :: [Attribute]
    } deriving (Show, Eq, Ord)

instance ToHtml a => ToHtml (Element a) where
    toHtml (Element {name, mcontent, attrs}) =
        with (makeElement name (maybe mempty toHtml mcontent)) attrs
    toHtmlRaw (Element {name, mcontent, attrs}) =
        with (makeElement name (maybe mempty toHtmlRaw mcontent)) attrs

-- | We see the <head> element not as @Html ()@, but as @Set (Html ())@ because
--
-- 1. \<head\> is an almost flat hierarchy, and
-- 2. we want 'HeadModT''s to be able to modify the elements in \<head\>, which would not be possible by storing it as @Html ()@, since @Html ()@, despite being a semigroup, has no way of accessing its elements separately.
--
-- One such need is not adding redundant elements; if I want to add three Google charts, I shouldn't add the CSS & JS three times! This is the reason for using @StateT@, too.
--
-- Like in the case of 'Element', you'll probably use @Head Text@ in most cases. Note that @a@ must equal @b@ for @Head a@, @Element b@, when using them for their ultimate purpose: 'toDoc' (keeping in mind that @HeadModT@ is build on @Head h@.)
type Head a = S.Set (Element a)

-- | Wrapper around elements; allows automatically adding to or modifying the \<head\> html element depending on which elements you use in a page. For instance, if you use a Google Charts element and a @CodeBox@ element in your \<body\>, then the \<head\> will automatically have imports for Prism's and Google Charts' CSS & Javascript \<link\>'s and \<script\>'s.
--
-- Example usage, where we put-in html for the author of a page, which automatically ensures that an author <\meta\> element is present in <\head\>:
--
-- @
-- example :: (Ord h, Monad m) => HeadModT h m ()
-- example = do
--
--     -- add \<meta author="Tom Smith"\> if said element does not already exist
--     putHead author
--
--     -- the html to render
--     ph $ h2_ "Author: Tom Smith"
--
--     where author = Element "meta" Nothing
--         [name_ "author", content_ "Tom Smith"]
-- 
-- test :: Html ()
-- test = toDoc toHtml head body
--     where
--         -- head is a Set; using -XOverloadedLists
--         head = [ Element "title" (Just ("Sample Page" :: T'.Text)) []
--                ]
--         body = do
--             example
--             ph $ div_ "here's a div!"
-- @
--
-- >>> renderText test
-- <!doctype html>
-- <html>
--     <head>
--         <meta content="Tom Smith" name="author"></meta>
--         <title>Sample Page</title>
--     </head>
--     <body>
--         <h2>Author: Tom Smith</h2>
--         <div>here&#39;s a div!</div>
--     </body>
-- </html>
--
-- Note the @do@ notation for subfunction @body@: we do @example *> ph (div_ "here's a div!")@; we could have instead done @ph (div_ "here's a div!") <* example@; the composition of @Html@ and @Head@s is associative, and is accomplished by collecting effects despite the sequence-with-discard functions @*\>@, @\<*@, @\>\>@, and @\<\<@.
newtype HeadModT h m a = HeadModT
    { runHeadModT :: StateT (Head h) (HtmlT m) a }
    deriving (Functor, Applicative, Monad)

instance (Semigroup a, Monad m) => Semigroup (HeadModT h m a) where
    HeadModT a <> HeadModT b = HeadModT $ liftA2 (<>) a b

instance (Monoid a, Monad m) => Monoid (HeadModT h m a) where
    mempty = HeadModT . lift . pure $ mempty

instance Monad m => IsString (HeadModT h m ()) where
    fromString = HeadModT . lift . fromString

type HeadMod h = HeadModT h Identity

-- | Put an item into a Head
putHead :: (Monad m, Ord h) => Element h -> HeadModT h m ()
putHead = HeadModT . modify . S.insert

-- | Merge a Head with another Head
mergeHead :: (Monad m, Ord h) => Head h -> HeadModT h m ()
mergeHead s = HeadModT $ modify (<> s)

-- | Lift 'HtmlT' into 'HeadModT'. Stands for "pure head."
ph :: Monad m => HtmlT m a -> HeadModT h m a
ph = HeadModT . lift

-- | The main way to use @HeadModT@; converts HeadModT into \<head\> and \<body\> and puts them under 'doctypehtml_'.
toDoc :: (ToHtml h, Monad m)
      => (Element h -> HtmlT m ()) -- ^ conv. You may think of it as @h -> HtmlT m ()@; @Element h@ is the domain because of a silly typechecking technicality
      -> Head h -- ^ the initial \<head\> before the head modding is done
      -> HeadModT h m a -- ^ the \<body\>
      -> HtmlT m ()
toDoc conv h0 hmt = do
    head <- lift $ evalHtmlT html_head -- eval the HtmlT m (Head h) to get m (Head h), then rewrap into HtmlT; this way we extract the (Head h) without writing the Html contents here.
    doctypehtml_ $ do {head_ $ foldMap conv head; body_ html}
    where
        html_head = execStateT (runHeadModT hmt) h0 -- :: HtmlT m (Head h)
        html = const () <$> html_head -- :: HtmlT m ()

-- TODO: I should make this throw an exception or return a warning somehow if a variable fails to lookup in the provided map
-- there should be an uncons monad that acts a little bit differently from Maybe
-- | Perform variable substitution on a Text. Remember that there's no need for variable substitution for Lucid-generated markup!
--
-- Uses simple bash-like syntax:
--
-- * @${var}@ will lookup @var@ in the given map and replace with its value;
-- * escape with a backslash to use a literal: @\${v}@ will become the literal string @${v}@
-- * variables may have spaces and special characters in their names (see example below)
-- * you MUST use the braces; @$var@ will be interpreted as a string literal without substitution!
--
-- Example:
--
-- @
-- TIO.putStrLn =<< varsub toHtmlRaw
--     [("are", pure "4"), ("sub me!", pure "6")]
--     "${are} \\${to NOT} \\$not ${{sub me!} ${sub me!}}"
-- @
--
-- returns @"4 ${to NOT} $not \<{sub me! not found\> 6}"@. (The angle brackets are part of the "not found" error message.)
--
-- === Bugs
--
-- * The sequence @\\$not@ is transformed into @$not@, even though the backslash /should be kept/ since variable substitution isn't performed unless there are surrounding curly braces! However, I don't know why someone would have said sequence in any document anyway, so I don't care to change the implementation to fix that.
-- * Also, note that nested substitution is odd. Again, I don't exactly see this as a problem, because who's going to do that anyway? It's just insensible. Still, just in case you're thinking about recursive applications or scraping arbitrary processed text...always good to consider a function totally~
-- * If you use @varsub@ for what it's designed for, and you don't terribly misalign your braces, then it should serve you; but if you try to break it, you will succeed. It is a total function, in that it terminates for any inputs, still.
varsub :: Monad m
       => M.Map T'.Text (HtmlT m ()) -- ^ variable substitution map
       -> (T'.Text -> HtmlT m ()) -- ^ conv
       -> T'.Text -- ^ input text
       -> HtmlT m () -- ^ output html
varsub m conv = go
    where
--      go :: T'.Text -> HtmlT m ()
        go t = case T'.break (\c -> c == '$' || c == '\\') t of
            (a,b) -> conv a <> case T'.uncons b of
                Nothing -> mempty
                Just ('\\', bs) -> case T'.uncons bs of
                    Nothing -> "\\"
                    Just (x,xs) -> conv (T'.singleton x) <> go xs
                Just ('$', bs) -> case T'.uncons bs of
                    Just ('{', bs') -> case T'.break (== '}') bs' of
                        (α, "") -> conv α
                        (α, β) -> fromMaybe (notFound α) (M.lookup α m) <> go (T'.tail β) -- tail is safe here; pattern match guarantees that β is not null
                    _ -> go bs
                _ -> error "Unreachable" -- c must be \ or $; guaranteed by first T'.break statement. Dummy pattern match here to let GHC know that this is a complete pattern match
            where
--              notFound :: T'.Text -> HtmlT m ()
                notFound x = with span_ [style_ "color:red"] (toHtml $ "<Oops: Variable \"" <> x <> "\" not in lookup table!>")

-- | Like 'varsub', except for more complex elements.
--
-- Example of a markdown document that features macros:
--
-- ==== Input File
--
-- @
-- ## Let's Example!
-- \@\@CodeBox -- I'm starting the macro! You may put comments here; all text folling the macro name, on the same line as the macro name, is discarded.
-- file1.txt
-- here are the contents
-- of file.
-- They will be put in a codebox.
-- ENDMACRO
--
-- * a bulleted item
--
-- \@\@macro1
-- arg1
-- arg2
-- ENDMACRO
--
-- \<footer\>that's all, folks\</footer\>
-- @
--
-- Then we'd process this input by the following code:
--
-- @
-- renderTextT (runMacros toHtmlRaw m0 input)
--     where
--         m0 = M.fromList -- in practice macros will be functions, usually each in their own module - λ's are not maintainable code!
--             [ ("macro", \\conv ts -> conv . T'.unlines $ fmap (\<\> \"HOHO\") ts) -- add \"HOHO\" to each line
--             , (\"CodeBox\", \\conv ts -> conv $ T'.unlines ts) -- leave the lines as they are if user chose raw output; otherwise the transformation is escaping special html characters
--             ]
-- @
--
-- Each macro like @CodeBox@ here has its own syntax. However, all macros share a common syntax: a macro begins with a double at-sign with no whitespace preceeding it, and its argument(s) is the text that begins on the following line, through to the next line beginning with the string literal \"ENDMACRO\" (again, with no preceeding nor trailing whitespace.)
--
-- ==== Notes
--
-- * The keys in runMacros's map (@m0@ in this example) do not begin with the double at-sign!
-- * A macro that takes no arguments is equivalent to a varsub variable substitution, except that the varsub may be used inline and the variable name may contain whitespace. Thus there's never a reason to have a nullary macro.
-- * Macro names /and/ the ENDMACRO string are case-sensitive. I recommend using either lowercase-with-hyphens or CamelCase for macro names.
-- * Also, note that the substitution begins on the line that the macro begins on; you may prefer a line of white space both before and after your macro definition, or not.
-- * The substitution macros should use either 'toHtml' or 'toHtmlRaw', depending on what's passed to @runMacros@.
runMacros :: (Monad m)
         => M.Map T'.Text (MacroT h m T'.Text) -- ^ macro map
         -> (T'.Text -> HtmlT m ()) -- ^ @toHtml@ or @toHtmlRaw@
         -> T'.Text -- ^ input text to transform
         -> HeadModT h m () -- ^ output html
runMacros m conv = go . T'.lines
    where
--      go :: [T'.Text] -> HeadModT h m ()
        go ts = case break ("@@" `T'.isPrefixOf`) ts of
            (a, b) -> ph (conv $ T'.unlines a) <> case b of
                [] -> mempty
                ((T'.takeWhile (not . isSpace) . T'.drop 2 -> macroName):ams) -> case span (/="ENDMACRO") ams of
                    (args, drop 1 -> bms) -> maybe (notFound macroName) (\f -> f conv args) (M.lookup macroName m) <> go bms
            where
--              notFound :: T'.Text -> HeadModT m ()
                notFound x = ph $ with span_ [style_ "color:red"] (toHtml $ "<Oops: Macro \"" <> x <> "\" not in lookup table!>")

-- | Turn a monadic variable into Html, so that it renders (rather than @lift@, which lifts in such a way that does not render)
--
-- Example:
--
-- >>> renderTextT $ liftRender (fromJust <$> lookupEnv "USER") <> liftRender (pure " ") <> liftRender (fromJust <$> lookupEnv "TERM")
-- "nic screen" :: IO Text
--
-- Notice the use of @liftRender (pure " ")@; @pure " "@ would be @:: (IsString s, ToHtml s) => HtmlT m s@, which is no good. After all, only HtmlT m () renders or is even a semigroup.
--
-- I want to highlight the difference between 'lift' and @liftRender@. Given the @IsString@ instance for @HtmlT m ()@, the above example (with OverloadedStrings) becomes:
--
-- @renderTextT $ liftRender (fromJust \<$\> lookupEnv \"USER\") \<\> " " \<\> liftRender (fromJust \<$\> lookupEnv \"TERM\")@
liftRender :: (Monad m, ToHtml a) => m a -> HtmlT m ()
liftRender = toHtml <=< lift -- originally I'd written: liftRender = HtmlT . fmap ((,()) . const . putStringUtf8) :: Functor m => m String -> HtmlT m ()

-- ** HTML Endomorphisms

fromcod :: ArrowApply a => (x -> a l e) -> a x e
fromcod k = proc x -> k x -<< undefined

-- | Change an XTag's tag or leave a node as-is
--
-- Convenience function
setTag :: String -> XNode -> XNode
setTag t = \case
    XTag _ ns -> XTag (mkName t) ns
    a -> a

-- | Derive a table of contents from @\<h1\>@, @\<h2\>@ etc. elements with or without an @id@ attribute
--
-- Use with 'giveIdsToHeadings' to ensure that the TOC links aren't broken!
--
-- *This is not an automorphism!* It returns the TOC element itself. If you want to insert the TOC back into the original document, you must do that yourself. Being an @ArrowList@ is a bit misleading here considering that the resulting list is a singleton.
deriveTOC :: (ArrowXml a, ArrowChoice a)
          => Bool -- derive id attributes for heading tags that don't already have id's? If @True@, then we can include /all/ headings in the TOC; if @False@, only headings having an "id" attribute are included.
          -> Bool -- ordered elements? @True@ => ol; @False@ => ul
          -> a XmlTree XmlTree
deriveTOC deriveAll (bool ("ul" :: String) "ol" -> listWrapper)
     = (deep isHeading >>> if deriveAll then deriveAddId `when` (neg $ hasAttr "id") else hasAttr "id")
    >>. toList . readIndentedGeneral
            (("Parse error for table of contents entry " <>) . T'.pack . show)
            (Right . headingToAnchorPair)
    >>> selem "li" [mkText <<^ T'.unpack] ||| arr listFold
    where
        -- convert <h_n id="idValue">innerHtml</h_n> to (n, <a href="#idValue">innerHtml</a>)
        headingToAnchorPair :: XmlTree -> (Int, XmlTree) -- needs to be XmlTree -> (Int, XNode)
        headingToAnchorPair (NTree (XTag h attrs) innerHtml) = (charToInt $ localPart h !! 1, NTree (XTag (mkName "a") (kat [] attrs)) innerHtml)
            where
                -- convert id to href, leaving other attrs as-is
                kat z0 (NTree (XAttr (localPart -> "id")) [NTree (XText ref) []]:xs) = kat ((NTree (XAttr $ mkName "href") [NTree (XText $ '#':ref) []]):z0) xs
                kat z0 (x:xs) = kat (x:z0) xs
                kat z0 [] = z0
        
                -- like read but for Char and faster
                charToInt :: Char -> Int
                charToInt c = ord c - 48

        -- transform tree of anchor elements into a TOC nested list
        listFold :: Tree.Tree XmlTree -> XmlTree
        listFold t = flip Tree.foldTree t $ \(wrapInLi -> liAnchor) -> \case
            [] -> liAnchor
            ls -> wrapInLr [liAnchor, wrapInLr ls] {- BUG: if ls is of listWrapper, don't wrap it!
                                                      Not really a /problem/, though, so I'll leave it as-is. -}
            where
                -- <li>'s always have exactly one child, even if that child is an <ol> or <ul>
                wrapInLi :: XmlTree -> XmlTree
                wrapInLi = NTree (XTag (mkName "li") []) . pure

                -- <ol/ul>'s will always contain one or more <li>'s.
                wrapInLr :: XmlTrees -> XmlTree
                wrapInLr = NTree (XTag (mkName listWrapper) [])

-- | Derive an id attribute (for a heading) from its innerHtml text content, such that the id has only alphanumerics and dashes whitelists.
--
-- I figure I'll export this here just in case anyone has any use for it. It's used for 'deriveTOC' and 'giveIdsToHeadings'
deriveAddId :: ArrowXml a => a XmlTree XmlTree
deriveAddId = proc x -> do
    t <- deep getText >. intercalate " " -< x
    addAttr "id" (foldr' tf mempty t) -<< x
    where
        tf :: Char -> String -> String
        tf (toLower -> c) t =
            if | isSpace c -> '-':t
               | inRange ('a','z') c || inRange ('0','9') c || c == '-' -> c:t
               | otherwise -> t

-- | Ensure that all headings have the @id@ attributes referenced by 'deriveTOC'
giveIdsToHeadings :: ArrowXml a => a XmlTree XmlTree
giveIdsToHeadings = processTopDown $ deriveAddId `when` (isHeading >>> neg (hasAttr "id"))

-- export this
-- | A little proofreading arrow: gets headings lacking the id attribute
noIdHeadings :: ArrowXml a => a XmlTree XmlTree
noIdHeadings = deep $ isHeading >>> neg (hasAttr "id")

-- defined as its own entity for easy re-use in deriveId and noIdHeadings
-- do not export
isHeading :: ArrowXml a => a XmlTree XmlTree
isHeading = isElem >>> hasNameWith ((\case ['h', n] -> isDigit n; _ -> False) . localPart)

-- | Transform \<pre\>\<code\> blocks into \<table\>s with line numbers.
--
-- Transformation only occurs when number of lines in code block is at least equal to threshold
--
-- Assumes that the first line in such a block is the least indented. If not so, define leadingFirstSpaces as
-- min (length . takeWhile isSpace <$> ts)
lineCode :: ArrowXml a => Int -> a XmlTree XmlTree
lineCode threshold = processTopDown $ g `when` ((isElem >>> hasName "pre") /> (isElem >>> hasName "code"))
    where
        g = applyA
             $  listA (deep getText >>^ lines >>^ filter (\case [] -> False; s -> not $ all isSpace s))
            >>> arr fold
            >>> arr (\case
                [] -> this
                ts ->
                    let leadingFirstSpaces = length (takeWhile isSpace (head ts))
                    in if (length ts < threshold) then this else
                        (selem "table"
                            [ selem "tbody" . fmap (\(i,t) -> selem "tr" [i,t]) $ zip
                                ((\i -> selem "td" [txt $ show i]) <$> [1..])
                                ((\t -> selem "td" [txt $ drop leadingFirstSpaces t]) <$> ts)
                            ]))

-- | For some reason, one can't do @with script_ [src_ ⋯] mempty@
--
-- Use @headscript_@ instead of 'script_' in the 'head_' function for this purpose
headscript_ :: T'.Text -> Html ()
headscript_ src = with (makeElement "script") [src_ src] mempty

-- * Helpers

-- | Transform a unary-parameterized constant arrow into an arrow whose domain is that parameter.
--
-- Useful for passing values from 'getText' into pamareterized arrows such as @arr (\t -> selem "p" [XText t])@
fromcod :: ArrowApply a => (x -> a l e) -> a x e
fromcod k = proc x -> k x -<< undefined

-- ToHtml a => [a] → <ul> → <nav>. Apply CSS rule nav {width:fit-content;height:fit-content}.

-- Common layouts for collections (Foldables):
-- - float
-- - list (horiz. or vert.) -- these are merely CSS classes ("vert" and "horiz") to apply to a <div> or w/e
-- - card layout

{-

I suppose all these really are html, except that they don't combine as the Html monad nor monoid do; they sometimes combine associatively.
-- should support semantic web extensions, too. This is, however, a bit of an emerging field, and so its standardization is fluctuating. The most general programming technique is having each semantic element correspond to a function that transforms data, and have all these transforms ordered by levels, so that composition is automatic:

Ideally this is: translating any document of a given *structure* into HTML (or any other kind of markup or other format of data; a morphism.)
HTML is already around; all we need is to keep our information in a given structure, and find a morphism from said structure to HTML.

For mixed content, let's just keep all attributes of all elements (minus a's href) to https.

-- see https://developer.mozilla.org/en-US/docs/Web/HTML/Link_types for blog sites

HTML5 semantic elements: section (usually of an <article>), <header>*, <footer>*, (*though header and footer aren't really semantic; they're positional,) <nav>, <aside> (e.g. the "info", "warning" sections of Dummies books,) <main> (usually wraps an <article> for blog posts,) <time> (e.g. <time datetime="2018-02-14 00:00">Valentines'</time> - useful for upcoming dates!)
-}
