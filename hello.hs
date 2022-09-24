import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
    html_
        "My title"
        (append_ (h1_ "Hello, world!") 
        (append_ (p_ "Let's learn about Haskell!") 
        (p_ "Safer HTML construction with types")))