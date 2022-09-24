main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
    html_
        "My title"
        (append_ (h1_ "Hello, world!") 
        (append_ (p_ "Let's learn about Haskell!") 
        (p_ "Safer HTML construction with types"))) 

html_ :: Title -> Structure -> Html
html_ title content = 
    Html 
        (el "html" (el "head" (el "title" title) <> el "body" (getStructureString content)))

h1_ :: String -> Structure
h1_ = Structure . el "h1"
p_ :: String -> Structure
p_ = Structure . el "p"

el :: String -> String -> String
el tag content = 
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

newtype Html = Html String
newtype Structure = Structure String

type Title = String

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

getStructureString :: Structure -> String
getStructureString (Structure str) = str

render :: Html -> String
render html =
    case html of
        Html str -> str
