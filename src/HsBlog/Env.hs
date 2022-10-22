{- | Environment values.
Define environment values to configure the conversion 
-}
module HsBlog.Env where

data Env
  = Env
    { eBlogName :: String -- ^ Represents the Html title
    , eStylesheetPath :: FilePath -- ^ Path to the styles file
    , eReplace :: Bool -- ^ Confirms replacement
    }
  deriving Show

{- | Default environment values:

- eBlogName = My Blog
- eStylesheetPath = style.css
- eReplace = false
-}
defaultEnv :: Env
defaultEnv = Env "My Blog" "style.css" False