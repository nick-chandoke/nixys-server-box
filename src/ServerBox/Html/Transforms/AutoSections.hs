-- | Transforms headings and subheadings in Markdown into sections and subsections:
--
-- @
-- ## Heading
-- some text
-- ### Subheading
-- more text
-- 
-- ### Subheading 2
-- ⋯
-- @
--
-- becomes
--
-- @
-- \<section\>
--     \<h2>Heading\</h2\>
--     some text
-- 
--     \<section\>
--         \<h3\>Subheading\</h3\>
--         more text
--     \</section\>
-- 
--     \<section\>
--         \<h3\>Subheading 2\</h3\>
--         ⋮
--     \</section\>
-- \</section\>
-- @
module ServerBox.Html.Transforms.AutoSections where

import Data.Char (isSpace)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tree
import ServerBox.Html (Macro)
import Lucid
import Control.Arrow ((***))
import NicLib.NStdLib ((>*>))
import NicLib.Tree (readIndentedGeneral)
import qualified Data.Text as T'

autoSections :: Macro T'.Text
autoSections convFn = (foldMap convFn *** foldMap (either convFn tc) . readIndentedGeneral id readHeading >*> (<>)) . break (isRight . readHeading)
    where
        readHeading :: Text -> Either (Html () -> Html ()) (Int, Html () -> Html ()) -- remember that, for readIndentedGeneral, readHeading needs to be :: full -> Either a (i, a)
        readHeading t = fromMaybe (Left (<> convFn t)) $ do -- this do block is :: Maybe (Either (Html () -> Html ()) (Int, Html () -> Html ()))
            ('#', ts) <- T'.uncons t
            let (succ . T'.length -> lvl, spaceAndHeading) = T'.span (=='#') ts -- headings begin with a hash
            (mustBeSpace, rest) <- T'.uncons spaceAndHeading -- and there's a mandatory whitespace between hashes and heading text
            if isSpace mustBeSpace then
                pure . (fromMaybe (\x -> Left $ (<> "Parsing error: " <> convFn x <> " has " <> convFn (T'.pack $ show lvl) <> " leading hashes; this does not correspond to any header HTML tag.")) $ hi lvl) $ T'.stripStart rest
            else
                Nothing
            where
                hi :: Int -> Maybe (T'.Text -> Either a (Int, Html () -> Html ()))
                hi i = let q h = pure (pure . (i,) . (\headingElement x -> section_ $ headingElement <> x)  . h . convFn) in case i of
                    1 -> q h1_
                    2 -> q h2_
                    3 -> q h3_
                    4 -> q h4_
                    5 -> q h5_
                    6 -> q h6_
                    _ -> Nothing

        tc :: Tree (Html () -> Html ()) -> Html ()
        tc (Node f c) = f $ foldMap tc c
