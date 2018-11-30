{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DataKinds, GADTs, KindSignatures, FlexibleInstances, TypeFamilies, TypeOperators  #-}

-- TODO: make function to AMPlify code (boilerplate, that is)
-- | Semantic HTML for webpages. Rather than be very general for HTML, this Html module encapsulates common HTML data patterns, and creates data types for them, such as radio or checkboxes, lists, etc. Makes markup less verbose, but still structured (as opposed to Markdown, which is non-hierarchical and isn't easily extended.)
--
-- Remember that this library uses /strict/ 'StateT'!
--
-- Throughout this library you'll see the identified @conv@(ersion function); this refers to either @toHtml@ or @toHtmlRaw@.
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
  -- * HTML Endo Helper Functions
, extractContent
, elementToText
, elementToTextInner
, elementToTagHtml
  -- * Extra Tags
, headscript_
) where

-- cmark-gfm
-- import CMarkGFM (commonmarkToHtml, extTable, extAutolink)

-- base
import Control.Applicative
import Control.Arrow (second, (>>>))
import Control.Monad ((<=<), (>=>))
import Data.Char (isSpace, toLower, ord)
import Data.Ix (inRange)
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Prelude hiding (Applicative(..))
import Data.Function (fix)

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

-- lucid
import Lucid
import Lucid.Base

-- containers
import Data.Sequence (Seq)
import Data.Tree (foldTree, Tree)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- miscellaneous packages
import qualified Data.Text as T' -- text
import Data.Text.Lazy (toStrict)
import qualified Text.XML as XML -- html-conduit
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

{-
do
    c <- TIO.readFile ""
    (if fmap T'.toLower . drop 1 $ T'.lines == ["<!doctype markdown>"] c then commonmarkToHtml [] [extTable, extAutolink] else id) . varsub . runMacros $ c
-}

{- My attempt at making it so that you don't need to use ph so often. WIP.
data HtmlLike m a = forall m a. ToHtml a => HtmlLike m a
instance Functor m => Functor (HtmlLike m) where fmap f (HtmlLike x) = HtmlLike $ f <$> toHtml x
instance Applicative m => Applicative (HtmlLike m) where
    pure = HtmlLike
    liftA2 f (HtmlLike x) (HtmlLike y) = HtmlLike $ liftA2 f (toHtml x) (toHtml y)
instance Monad HtmlLike where
    HtmlLike x >>= f = toHtml x >>= f
-}

-- So remember that when writing, use lift, and when using, use ph. If you know of a non-convoluted way to, in a do-block, automatically lift Html into either StateT or HeadModT, please contact me about it or open a pull request. Obviously I could instance classes such as Term, but that doesn't help because do-notation/(>>=) requires that everything be in the same monad.

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

-- HTML Endomorphisms -- TODO: rewrite using Text.XML.Stream.Parse/Conduit

-- | Transform \<pre\>\<code\> blocks into \<table\>s with line numbers.
-- see 'Text.HTML.DOM' for parsing stringlike's into a @Document@
lineCode :: Int -- ^ threshold; code must be at least this many lines in order to be numbered
         -> XML.Document
         -> Html ()
lineCode threshold (XML.documentRoot -> doc) = elementToTagHtml doc $ foldMap f (XML.elementNodes doc)
    where
        f :: XML.Node -> Html ()
        f (XML.NodeElement e) = let untransformed = elementToTagHtml e $ foldMap f (XML.elementNodes e) in
            if XML.elementName e == "pre" then
                case XML.elementNodes e of
                    -- filter nodes for content or element;
                    -- convert elements to their text representations;
                    -- concatenate all text nodes into one; then g can process it.
                    [XML.NodeElement code] -> g . foldMap (\case XML.NodeContent t -> t; XML.NodeElement e -> elementToText e; _ -> mempty) $ XML.elementNodes code
                    _ -> untransformed
            else untransformed
        f (XML.NodeContent t) = toHtml t
        f _ = mempty

        -- used in a foldMap over children nodes of <pre><code>'s
        g :: T'.Text -> Html ()
        g t@(T'.lines -> ts) = let {lc = flip with [class_ "linedCode"]; tsl = length ts} in
            if tsl > 0 && tsl >= threshold then
                let !lw = T'.length (T'.takeWhile isSpace $ head ts)
                    -- commonmarkToHtml doesn't remove leading whitespace when converting indented code blocks to HTML.
                    -- Because we're expecting CSS rule .linedCode td {white-space:pre}, we need to remove that leading whitespace.
                in lc . table_ . tbody_ . snd $ foldl'
                    (\(i,acc) v -> (succ i, acc <> (tr_ $ td_ (toHtmlRaw $ show i) <> td_ (toHtmlRaw $ T'.drop lw v))))
                    (1, mempty)
                    ts
            else
                pre_ . lc . code_ $ toHtmlRaw t

-- | Structure created just for @deriveTOC@
data TOCEntry = TOCEntry { level :: !Int, ref :: T'.Text, innerHtml :: Html () } deriving Show

-- | Derive a table of contents from @\<h1\>@, @\<h2\>@ etc. elements with an @id@ attribute
--
-- Suggested use: @with section_ [id="toc"] $ h2_ "Contents" \<\> snd (deriveTOC True doc)@
--
-- Note that if you set @True@ for the 1<sup>st</sup> parameter, heading elements that already have @id@ fields will keep their original respective fields
--
-- Setting @True@ vs. @False@ really changes the purpose of @deriveTOC@; if @False@, it's more of a proofreading tool; when @True@, it describes an HTML document.
--
-- prop> == deriveAll == @True@ ⇒ fst (deriveTOC _) == mempty
deriveTOC :: Bool -- ^ derive & add @id@ attribute to @id@-less heading elements, and include them in the TOC?
          -> (Html () -> Html ()) -- ^ either @ul_@ or @ol_@
          -> XML.Document
          -> (Seq T'.Text, Html ())
deriveTOC deriveAll listWrapper = second postProcess . foldMap f . XML.elementNodes . XML.documentRoot
    where
        postProcess :: Seq TOCEntry -> Html ()
        postProcess = readIndentedGeneral (("Parse error for table of contents entry " <>) . T'.pack . show)
            (\case TOCEntry {level, ref, innerHtml} -> Right (level, li_ $ with a_ [href_ $ "#" <> ref] innerHtml)) -- this <li><a> will be wrapped in either <ol>, <ul>; see listFold
                -- always Right b/c we're processing headings only, all of which must have a level
            >>> foldMap (either (with span_ [class_ "parseError"] . toHtmlRaw) (foldTree listFold)) -- :: Seq (Either Text (Tree (Html ()))) -> Html ()
            >>> listWrapper -- remember to wrap it all up into one node!

        -- fold a tree of <li> elements into one of listWrapper-wrapped <li> sequences
        listFold :: Html () -> [Html ()] -> Html ()
        listFold n lis =
            case length lis of
                0 -> n -- 0 means that this is a <li> node without children; therefore don't use listWrapper
                1 -> n <> head lis
                _ -> n <> listWrapper (fold lis)

        -- | try to read a tag (i.e. nameLocalName) as a heading, returning its level if so
        tagAsHeading :: T'.Text -> Maybe Int
        tagAsHeading = T'.uncons >=> (\(c,cs) -> T'.uncons cs >>= \((flip (-) 48) . ord -> d,_) -> if toLower c == 'h' && inRange (1,6) d then Just d else Nothing)

        f :: XML.Node -> (Seq T'.Text, Seq TOCEntry)
        f (XML.NodeElement e@(XML.Element {XML.elementName = XML.Name {XML.nameLocalName = nln}, XML.elementAttributes, XML.elementNodes})) =
            let headingLink idAttr lvl = (mempty,) . Seq.singleton . TOCEntry lvl idAttr . toHtmlRaw $ foldMap elementToTextInner elementNodes
            in case tagAsHeading nln of
                Nothing -> foldMap f elementNodes -- not a heading tag; continue *tree* (not list) traversal to search for more heading tags
                Just lvl -> case elementAttributes M.!? "id" of
                    Nothing ->
                        if deriveAll then
                            headingLink (deriveId e) lvl
                        else
                            (Seq.singleton $ elementToText e, mempty)
                    Just idAttr ->
                        headingLink idAttr lvl
        f _ = mempty

        -- derive an @id@ attribute (for a heading) from its inner text content, such that the id has only alphanumerics and dashes
        deriveId :: XML.Element -> T'.Text
        deriveId = T'.foldl' tf mempty . T'.strip . extractContent . XML.NodeElement
            where
                tf t (toLower -> c) =
                    if | isSpace c -> T'.snoc t '-'
                       | inRange ('a','z') c || inRange ('0','9') c || c == '-' -> T'.snoc t c
                       | otherwise -> t

-- HTML Endomorphism Utilities --

-- | Extract & concat only NodeContent
extractContent :: XML.Node -> T'.Text
extractContent (XML.NodeElement (XML.Element {XML.elementNodes})) = foldMap extractContent elementNodes
extractContent (XML.NodeContent t) = t
extractContent _ = mempty

-- totally render an element's, including its tag, attributes, and children
-- used inside rendering children of <pre><code> blocks to ensure that HTML encoded in those blocks is untouched
elementToText :: XML.Element -> T'.Text
elementToText e = toStrict . renderText . elementToTagHtml e . toHtmlRaw . foldMap elementToTextInner $ XML.elementNodes e

-- | Like 'elementToText', but don't render the element; render only its inner HTML
elementToTextInner :: XML.Node -> T'.Text
elementToTextInner = fix $ \go -> \case
    XML.NodeElement e -> foldMap go (XML.elementNodes e)
    XML.NodeContent t -> t
    XML.NodeComment c -> "<!-- " <> c <> " -->"
    _ -> mempty

-- render an element's tag with attributes, allowing one to pass-in inner html
elementToTagHtml :: XML.Element -- ^ element to transform
                 -> Html () -- ^ inner/child html
                 -> Html ()
elementToTagHtml (XML.Element {XML.elementName = XML.Name {XML.nameLocalName = nln}, XML.elementAttributes}) inner =
    with (makeElement nln inner) $ M.foldlWithKey' (\list attr val -> makeAttribute (XML.nameLocalName attr) val :list) [] elementAttributes

---------------------------------

-- | for some reason, one can't do @with script_ [src_ ⋯] mempty@
--
-- use @headscript_@ instead of 'script_' in the 'head_' function for this purpose
headscript_ :: T'.Text -> Html ()
headscript_ src = with (makeElement "script") [src_ src] mempty

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
