module Html.Internal where
import Numeric.Natural ( Natural )

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
    Html
    (el "html" (el "head" (el "title" title) <> el "body" (getStructureString content)))

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h_ :: Natural -> String -> Structure
h_ number = Structure . el ("h" <> show number) . escape

h1_ :: String -> Structure
h1_ = h_ 1 . escape

ul_ :: [Structure] -> Structure
ul_ = list "ul"

ol_ :: [Structure] -> Structure
ol_ = list "ol"

empty_ :: Structure
empty_ = Structure ""

instance Semigroup Structure where
    (<>) c1 c2 = Structure (getStructureString c1 <> getStructureString c2)
    
instance Monoid Structure where
  mempty = empty_

-- * Render

render :: Html -> String
render html =
    case html of
        Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure str) = str

escape :: String -> String
escape =
    let
    escapeChar c =
        case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
    in
    concat . map escapeChar

list :: String -> [Structure] -> Structure
list tag = Structure . el tag . concat . map (el "li" . getStructureString)