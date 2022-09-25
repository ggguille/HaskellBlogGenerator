import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
    html_
        "My title"
        (append_ (h1_ "Hello, world!") 
        (append_ (p_ "Let's learn about Haskell!") 
        (append_ (p_ "Escaping <characters>")
        (append_ (ul_ [ p_ "Unordered lists"
        , p_ "Ordered lists"
        , p_ "Code blocks"
        ])
        (append_ (ol_ [ p_ "Unordered lists"
        , p_ "Ordered lists"
        , p_ "Code blocks"
        ])
        (code_ "Code blocks") )))))