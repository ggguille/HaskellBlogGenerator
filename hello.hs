import Html
import Markup

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
    html_
        "My title"
        (
            h1_ "Hello, world!" 
            <> p_ "Let's learn about Haskell!"
            <> p_ "Escaping <characters>"
            <> ul_ [ 
                p_ "Unordered lists"
                , p_ "Ordered lists"
                , p_ "Code blocks"
            ]
            <> ol_ [ 
                p_ "Unordered lists"
                , p_ "Ordered lists"
                , p_ "Code blocks"
            ]
            <> code_ "Code blocks"
        )