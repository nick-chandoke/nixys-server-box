-- | Create a @CodeBox@ from text. The syntax is
--
-- @
-- \@\@CodeBox
-- filename | language
-- file contents for this and remaining lines
-- ⋮
-- ENDMACRO
-- @
--
-- Example:
--
-- @
-- \@\@CodeBox
-- sample-file.hs | haskell
-- module Example where
-- ⋮
-- ENDMACRO
-- @
--
-- The language variable is used by prism for syntax highlighting, and is optional.
module ServerBox.Markup.Macros.CodeBox (codebox) where

import Control.Arrow ((***))
import Data.List (uncons)
import ServerBox.Markup (Macro)
import ServerBox.Markup.Elements.CodeBox
import qualified Data.Text as T'

codebox :: Ord h
        => (T'.Text -> T'.Text) -- ^ root a relative URI to CDN
        -> Macro h (Code 'WholeSeg, Maybe T'.Text)
codebox root conv ts = case uncons ts of
    Nothing -> mempty
    Just (process -> (filename, lang), contents) ->
        toHeadModT root conv (WholeCode filename (T'.unlines contents), if T'.null lang then Nothing else Just lang)
    where
        process = (T'.stripEnd *** T'.stripStart . T'.drop 1) . T'.break (=='|')
