{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DataKinds, GADTs, KindSignatures, FlexibleInstances, TypeFamilies, TypeOperators  #-}

-- TODO: make function to AMPlify code (boilerplate, that is)
-- | Semantic HTML for webpages. Rather than be very general for HTML, this Html module encapsulates common HTML data patterns, and creates data types for them, such as radio or checkboxes, lists, etc. Makes markup less verbose, but still structured (as opposed to Markdown, which is non-hierarchical and isn't easily extended.)
--
-- If using Markdown or some other non-html markup, you should consider whether to use @toHtml@ or @toHtmlRaw@!
module ServerBox.Html where

import CMarkGFM (commonmarkToHtml, extTable, extAutolink)
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Lucid
import Lucid.Base
import Prelude hiding (Applicative(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T'

type MacroT m a = (a -> HtmlT m ()) -> [T'.Text] -> HtmlT m ()
type Macro a = MacroT Identity a

runTransform :: (a -> HtmlT m ()) -> MacroT m a -> T'.Text -> HtmlT m ()
runTransform convFn m = m convFn . T'.lines

deriving instance Ord Attribute

-- | Abstract representation of an Html element that belongs in <head>. Usually I expect you'll use @Element Text@; however, I'm leaving it polymorphic so that you can use any @ToHtml a => Element a@, as that's the most general form of element that works with @toDoc@.
--
-- @Text@ should work fine considering that, as far as I know, at least, there are no children of <head> that have any more children than merely a single text node.
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
-- example = HeadModT $ do
--     modify $ S.insert author -- add \<meta author="Tom Smith"\> if said element does not already exist
--     lift $ h2_ "Author: Tom Smith" -- the html to render. Note that I'm using @lift@ rather than @ph@, because this do-block is StateT; the whole block is passed to the HeadModT constructor.
--     where author = Element "meta" Nothing [name_ "author", content_ "Tom Smith"]
-- 
-- test = flip (toDoc toHtml) (S.singleton $ Element "title" (Just "Sample Page" :: Maybe T'.Text) []) $ do
--     example
--     ph $ div_ "here's a div!" -- Note I'm using @ph@ here rather than @lift@, because this do-block is HeadModT!
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
newtype HeadModT h m a = HeadModT
    { runHeadModT :: StateT (Head h) (HtmlT m) a }
    deriving (Functor, Applicative, Monad)
type HeadMod h = HeadModT h Identity

-- | Lift 'HtmlT' into 'HeadModT'. Stands for "pure head."
ph :: Monad m => HtmlT m a -> HeadModT h m a
ph = HeadModT . lift

-- | The main way to use @HeadModT@; converts HeadModT into \<head\> and \<body\> and puts them under 'doctypehtml_'.
--
-- This being said, if you choose to use @HeadModT@, /all/ of your \<body\> nodes must be @HeadModT@!
toDoc :: (ToHtml h, Monad m)
      => (Element h -> HtmlT m ()) -- ^ either @toHtml@ or @toHtmlRaw@. You may think of it as @h -> HtmlT m ()@; @Element h@ is the domain because of a silly typechecking technicality
      -> HeadModT h m a
      -> Head h -- ^ the Head to translate into the document's <head> element
      -> HtmlT m ()
toDoc conv hmt h0 = do
    head <- lift $ evalHtmlT html_head -- eval the HtmlT m (Head h) to get m (Head h), then rewrap into HtmlT; this way we extract the (Head h) without writing the Html contents here.
    doctypehtml_ $ do {head_ $ foldMap conv head; body_ html}
    where
        html_head = execStateT (runHeadModT hmt) h0 -- :: HtmlT m (Head h)
        html = const () <$> html_head -- :: HtmlT m ()

{-
do
    c <- TIO.readFile ""
    (if fmap T'.toLower . drop 1 $ T'.lines == ["<!doctype markdown>"] c then commonmarkToHtml [] [extTable, extAutolink] else id) . varsub . blocksub $ c
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
-- TIO.putStrLn =<< varsub
--     [("are", pure "4"), ("sub me!", pure "6")]
--     "${are} \\${to NOT} \\$not ${{sub me!} ${sub me!}}"
-- @
--
-- --> "4 ${to NOT} $not \<{sub me! not found\> 6}". (The angle brackets are part of the "not found" error message.)
--
-- === Bugs
--
-- * The sequence @\\$not@ is transformed into @$not@, even though the backslash /should be kept/ since variable substitution isn't performed unless there are surrounding curly braces! However, I don't know why someone would have said sequence in any document anyway, so I don't care to change the implementation to fix that.
-- * Also, note that nested substitution is odd. Again, I don't exactly see this as a problem, because who's going to do that anyway? It's just insensible. Still, just in case you're thinking about recursive applications or scraping arbitrary processed text...always good to consider a function totally~
-- * If you use @varsub@ for what it's designed for, and you don't terribly misalign your braces, then it should serve you; but if you try to break it, you will succeed. It is a total function, in that it terminates for any inputs, still.
varsub :: M.Map T'.Text (HtmlT IO ()) -- ^ variable substitution map
       -> Bool -- ^ raw output?
       -> T'.Text -- ^ input text
       -> HtmlT IO () -- ^ output html
varsub m (bool toHtml toHtmlRaw -> conv) = go
    where
        go :: T'.Text -> HtmlT IO ()
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
                notFound :: T'.Text -> HtmlT IO ()
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
-- renderTextT (blocksub True m0 input)
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
-- * The keys in blocksub's map should not begin with the double at-sign!
-- * A macro that takes no arguments is equivalent to a varsub variable substitution, except that the varsub may be used inline and the variable name may contain whitespace. Thus there's never a reason to have a nullary macro.
-- * Macro names and the ENDMACRO string are case-sensitive. I recommend using either lowercase-with-hyphens or CamelCase for macro names.
-- * Also, note that the substitution begins on the line that the macro begins on; you may prefer a line of white space both before and after your macro definition, or not.
-- * The substitution macros should use either 'toHtml' or 'toHtmlRaw', depending on what's passed to @blocksub@.
blocksub :: (Monad m)
         => M.Map T'.Text (MacroT m T'.Text) -- ^ macro map
         -> Bool -- ^ raw output?
         -> T'.Text -- ^ input text to transform
         -> HtmlT m () -- ^ output html
blocksub m (bool toHtml toHtmlRaw -> conv) = go . T'.lines
    where
--      go :: [T'.Text] -> HtmlT m ()
        go ts = case break ("@@" `T'.isPrefixOf`) ts of
            (a, b) -> conv (T'.unlines a) <> case b of
                [] -> mempty
                ((T'.takeWhile (not . isSpace) . T'.drop 2 -> macroName):ams) -> let (args, drop 1 -> bms) = span (/="ENDMACRO") ams in maybe (notFound macroName) (\f -> f conv args) (M.lookup macroName m) <> go bms
            where
--              notFound :: T'.Text -> HtmlT m ()
                notFound x = with span_ [style_ "color:red"] (toHtml $ "<Oops: Macro \"" <> x <> "\" not in lookup table!>")

-- | Turn a monadic variable into Html, so that it renders (rather than @lift@, which lifts in such a way that does not render)
--
-- Example:
--
-- >>> renderTextT $ lift' (fromJust <$> lookupEnv "USER") <> lift' (pure " ") <> lift' (fromJust <$> lookupEnv "TERM")
-- "nic screen" :: IO Text
--
-- Notice the use of @lift' (pure " ")@; @pure " "@ would be @:: (IsString s, ToHtml s) => HtmlT m s@, which is no good. After all, only HtmlT m () renders or is even a semigroup.
--
-- I want to highlight the difference between 'lift' and @lift'@. Given the @IsString@ instance for @HtmlT m ()@, the above example (with OverloadedStrings) becomes:
--
-- @renderTextT $ lift' (fromJust <$> lookupEnv \"USER\") <> " " <> lift' (fromJust <$> lookupEnv \"TERM\")@
lift' :: (Monad m, ToHtml a) => m a -> HtmlT m ()
lift' = toHtml <=< lift -- originally I wrote: lift' = HtmlT . fmap ((,()) . const . putStringUtf8) :: Functor m => m String -> HtmlT m ()

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
