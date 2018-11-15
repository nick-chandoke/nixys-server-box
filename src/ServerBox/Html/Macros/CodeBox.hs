-- | Create a @CodeBox@ (works only with the @WholeCode@ constructor.) The syntax is
-- @
-- \@\@CodeBox
-- filename | language
-- file contents for this and remaining lines
-- ⋮
-- ENDMACRO
-- @
-- Example:
-- @
-- \@\@CodeBox
-- sample-file.hs | haskell
-- module Example where
-- ⋮
-- ENDMACRO
-- @
-- The language variable is used by prism for syntax highlighting, and is optional.
module ServerBox.Html.Macros.CodeBox (codeboxA) where

import Control.Arrow ((***))
import Data.List (uncons)
import ServerBox.Html (Macro)
import ServerBox.Html.Elements.CodeBox
import qualified Data.Text as T'

codeboxA :: Macro (Code 'WholeSeg, Maybe T'.Text)
codeboxA convFn ts = case uncons ts of
    Nothing -> mempty
    Just (process -> (filename, lang), contents) ->
        convFn (WholeCode filename (T'.unlines contents), if T'.null lang then Nothing else Just lang)
    where
        process = (T'.stripEnd *** T'.stripStart . T'.drop 1) . T'.break (=='|')
