-- | Create boxes that hold code or file contents. Syntax highlighting powered by <https://prismjs.com prism>.
--
-- Example:
--
-- @
-- WholeCode "sample.txt" "here's one part of the file, like lines 1 through 18"
-- <>: PartCode "here's another part, e.g. lines 100 through 200"
-- <>: PartCode "and yet another part, for instance the last section of an INI file (except that this example is just text, not an ini file ;p)"
-- @
module ServerBox.Markup.Elements.CodeBox
( CodeSegment (..)
, (<>:)
, Code (..)
, toHeadModT
) where

import Data.Set (Set)
import Lucid
import ServerBox.Markup (HeadModT, Element(..), liftHtml, mergeHead)
import qualified Data.Text as T'

-- | For posting code or textfile literals
data CodeSegment = WholeSeg | PartSeg

-- | semigroupoid instance for CodeSegment (as in defining arrows ad-hoc; I still don't quite understand how or why Haskell's Category class or ekmett's Semigroupoid is defined by transitivity.)
type family (a :: CodeSegment) :<>: (b :: CodeSegment) :: CodeSegment where
    'WholeSeg :<>: 'PartSeg = 'WholeSeg
    'PartSeg :<>: 'PartSeg = 'PartSeg

-- | cons for @Code@
infixl 5 <>:
(<>:) :: Code a -> Code 'PartSeg -> Code (a :<>: 'PartSeg) -- because the second argument is kind * this doesn't matter, but if (<>:) were :: Code a -> Code b -> Code (a :<>: b), then GHC would not fail to typecheck Code ('WholeSeg :<>: 'WholeSeg), even though there's no type instance for 'WholeSeg :<>: 'WholeSeg; and this bothers me! Why does GHC not say something along the lines of "there doesn't exist RHS of 'WholeSeg :<>: 'WholeSeg"?
WholeCode title c1 <>: PartCode c2 = WholeCode title (c1 <> "\n" <> c2) -- wrap
PartCode c1 <>: PartCode c2 = PartCode (c1 <> "\n⋮\n" <> c2)

-- how the hell do I haddock this correctly?!
data Code :: CodeSegment -> * where
    WholeCode :: T'.Text --  file/segment name
              -> T'.Text --  segment contents
              -> Code 'WholeSeg
    PartCode :: T'.Text --  segment contents
             -> Code 'PartSeg

-- just used for ToHtml below
q :: (Monad m) => (T'.Text -> HtmlT m ()) -> (Code 'WholeSeg, Maybe T'.Text) -> HtmlT m ()
q qf (WholeCode (qf -> title) (qf -> contents), m_lang) = with div_ [class_ "snippet"] $ do
    with (span_ title) [class_ "codebox-title"]
    pre_ $ maybe contents (\lang -> with code_ [class_ $ "language-" <> lang] contents) m_lang

-- | I don't care to bind language specification to the Code definition, since I don't anticipate that mixing code languages will be a likely error (in fact, it often is not an error)
-- use @Nothing@ for plantext
instance ToHtml (Code 'WholeSeg, Maybe T'.Text) where
    toHtml = q toHtml
    toHtmlRaw = q toHtmlRaw

toHeadModT :: (t ~ (Code 'WholeSeg, Maybe T'.Text), Ord h, Monad m)
           => (T'.Text -> T'.Text) -- ^ root a relative URI to CDN
           -> (t -> HtmlT m ()) -- ^ conv
           -> t
           -> HeadModT h m ()
toHeadModT root conv a = liftHtml (conv a)
    *> mergeHead ([ Element "link"   Nothing [href_ $ root "prism.css", rel_ "stylesheet"]
                 , Element "script" Nothing [src_ $ root "prism.js"]
                 ] :: Ord h => Set (Element h))
