{-# language
  DataKinds
, FlexibleInstances
, GADTs
, KindSignatures
, NamedFieldPuns
, OverloadedStrings
, ParallelListComp
, TypeFamilies
, TypeOperators
#-}

-- | Semantic HTML for webpages. Rather than be very general for HTML, this Html module encapsulates common HTML data patterns, and creates data types for them, such as radio or checkboxes, lists, etc. Makes markup less verbose, but still structured (as opposed to Markdown, which is non-hierarchical and isn't easily extended.)
--
-- === Nota Bene
--
-- * this library uses /strict/ 'StateT'!
-- * throughout the library you'll see the symbol @conv@; this refers to either @toHtml@ or @toHtmlRaw@.
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
, liftHtml
, (<+)
, (+>)
  -- * Macros
, MacroT
, Macro
, runMacros
  -- ** HTML Endomorphisms
  -- * Utilities
, varsub
, liftRender
  -- * Extra Tags
, headscript_
  -- * HXT-Based Utilities
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
import Data.String
import Control.Exception (SomeException)
import Data.Bool (bool)
import Data.Char (isSpace, isDigit, toLower, ord)
import Data.Foldable (toList, foldr', fold)
import Data.Ix (inRange)
import Data.List (intercalate)

-- hxt
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import qualified Text.XML.HXT.DOM.ShowXml as SX

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

-- lucid
import Lucid
import Lucid.Base

-- containers
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as Tree

-- miscellaneous packages
import qualified Data.Text as T' -- text
import Data.Text.Lazy (toStrict)

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
-- thing = liftHtml (div_ mempty)
--       *> 'mergeHead' ([ Element "link"   Nothing [href_ "some.css", rel_ "stylesheet"]
--                      , Element "script" Nothing [src_ "some.js"]
--                      ] :: Ord h => Set (Element h))
-- @
--
-- Well, that, /and/ both 'mergeHead' and 'putHead' require @Ord@ constraints as well! Thus there's a need for @Ord h@ in @thing@'s type signature.
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
-- 2. we want 'HeadModT''s to be able to modify the elements in \<head\>, which would not be possible by storing it as @Html ()@, since @Html ()@, despite being a semigroup, has no way of accessing its elements separately (/i.e./ it supports reduction but not generation.)
--
-- One such need is not adding redundant elements; if I want to add three Google charts, I shouldn't add the CSS & JS three times! This is the reason for using @StateT@, too.
type Head a = S.Set (Element a)

-- | Attach to some @Html a@ an automorphism on \<head\>. Allows defining html fragments in the same place as the resources that it requires; never worry about \<head\> again! More generally, useful for importing a bunch of dependencies non-redundantly and without conflict. Example:
--
-- @
-- -- | An \<h2\> tag as per usual Lucid, plus the side effect of adding an /author/ meta tag in \<head\>
-- authorTag :: (Ord h, Monad m) => Text -> HeadModT h m ()
-- authorTag name
--     = 'putHead' ('Element' "meta" Nothing [name_ "author", content_ name])
--    \<+ h2_ ("Article written by " \<\> name)
-- 
-- testDoc :: Html ()
-- testDoc = 'toDoc' toHtml head body
--   where
--     -- head is a Set; using -XOverloadedLists
--     head = [ Element "title" (Just ("Sample Page" :: T'.Text)) [] ]
--     body = authorTag "A.K. Yearling" <+ div_ "Ahuizotl's up to his old tricks again!"
-- @
--
-- >>> renderText testDoc
-- <!doctype html>
-- <html>
--     <head>
--         <meta content="A.K. Yearling" name="author"></meta>
--         <title>Sample Page</title>
--     </head>
--     <body>
--         <h2>Article written by A.K. Yearling</h2>
--         <div>Ahuizotl&#39;s up to his old tricks again!</div>
--     </body>
-- </html>
--
-- @(*>)@, @(+>)@, and friends are associative. I still haven't found a way to elegantly compose @(HeadModT a)@'s and @(Html a)@'s. Also, these examples are short and use @(<+)@; we could have used @do@ blocks and 'liftHtml' instead.
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

-- | Put an element into \<head\>
putHead :: (Monad m, Ord h) => Element h -> HeadModT h m ()
putHead = HeadModT . modify . S.insert

-- | Set union a @Head h@ with the @HeadModT@'s @Head h@ state
mergeHead :: (Monad m, Ord h) => Head h -> HeadModT h m ()
mergeHead s = HeadModT $ modify (<> s)

-- | Lift 'HtmlT' into 'HeadModT'
liftHtml :: Monad m => HtmlT m a -> HeadModT h m a
liftHtml = HeadModT . lift

-- | Lift LHS into @HeadModT@, then sequence
(+>) :: Monad m => HtmlT m a -> HeadModT h m b -> HeadModT h m b
a +> b = liftHtml a *> b

-- | Lift RHS into @HeadModT@, then sequence
(<+) :: Monad m => HeadModT h m b -> HtmlT m a -> HeadModT h m b
(<+) = flip (+>)

-- | The main way to use @HeadModT@; converts HeadModT into \<head\> and \<body\> and puts them under 'doctypehtml_'.
toDoc :: (ToHtml h, Monad m)
      => (Element h -> HtmlT m ()) -- ^ @conv@. You may think of it as @h -> HtmlT m ()@; @Element h@ is the morphism's domain because of a silly typechecking technicality
      -> Head h -- ^ the initial \<head\> before the head automorphisms are evaluated
      -> HeadModT h m a -- ^ the markup to set as child of the \<body\> element (do not wrap your HeadModT in a \<body\> tag!)
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
         => M.Map T'.Text (MacroT h m T'.Text) -- ^ macro identifiers and their corresponding functions
         -> (T'.Text -> HtmlT m ()) -- ^ @conv@
         -> T'.Text -- ^ input text
         -> HeadModT h m () -- ^ output html
runMacros m conv = go . T'.lines
    where
--      go :: [T'.Text] -> HeadModT h m ()
        go ts = case break ("@@" `T'.isPrefixOf`) ts of
            (a, b) -> liftHtml (conv $ T'.unlines a) <> case b of
                [] -> mempty
                ((T'.takeWhile (not . isSpace) . T'.drop 2 -> macroName):ams) -> case span (/="ENDMACRO") ams of
                    (args, drop 1 -> bms) -> maybe (notFound macroName) (\f -> f conv args) (M.lookup macroName m) <> go bms
            where
--              notFound :: T'.Text -> HeadModT m ()
                notFound x = liftHtml $ with span_ [style_ "color:red"] (toHtml $ "<Oops: Macro \"" <> x <> "\" not in lookup table!>")

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

-- | For some reason, one can't do @with script_ [src_ ⋯] mempty@
--
-- Use @headscript_@ instead of 'script_' in the 'head_' function for this purpose
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
