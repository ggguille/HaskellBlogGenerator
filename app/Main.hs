-- | Entry point for the hs-blog-gen program
module Main where

import OptParse
import qualified HsBlog

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO
import Control.Exception (bracket)

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output env ->
      HsBlog.convertDirectory env input output

    ConvertSingle input output replace ->
      let
        -- Here, action is the next steps we want to do.
        -- It takes as input the values we produce,
        -- uses it, and then returns control for us to clean-up
        -- afterwards.
        withInputHandle :: (String -> Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin ->
              action "" stdin
            InputFile file ->
              bracket
                (openFile file ReadMode)
                hClose
                (action file) 
      
        -- Note that in both functions our action can return any `a`
        -- it wants.
        withOutputHandle :: (Handle -> IO a) -> IO a
        withOutputHandle action =
          case output of
            Stdout ->
              action stdout
            OutputFile file -> do
              exists <- doesFileExist file
              shouldOpenFile <-
                if exists && not replace
                  then confirm
                  else pure True
              if shouldOpenFile
                then
                  bracket (openFile file WriteMode) hClose action
                else
                  exitFailure
      in
        withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)

------------------------------------------------
-- * Utilities

-- | Confirm user action
confirm :: IO Bool
confirm =
  putStrLn "File already exist and it will be replaced. Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> putStrLn "Invalid response. use y or n" *>
          confirm