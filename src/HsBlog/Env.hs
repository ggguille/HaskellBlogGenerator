module HsBlog.Env where

data Env
  = Env
    { eBlogName :: String
    , eStylesheetPath :: FilePath
    , eReplace :: Bool
    }
  deriving Show

defaultEnv :: Env
defaultEnv = Env "My Blog" "style.css" False