module HsBlog.Markup where
    
import Numeric.Natural ( Natural )
import Data.Maybe

type Document = [Structure]

{- | Represents a single markup structure. Such as:

- A paragraph
- An unordered list
- A code block
-}
data Structure
  = Heading Natural String
  -- ^ A section heading with a level
  | Paragraph String
  -- ^ A paragraph
  | UnorderedList [String]
  -- ^ An unordered list of strings
  | OrderedList [String]
  -- ^ An ordered list of strings
  | CodeBlock [String]
  -- ^ A code block

{- | Parse file content to Markup Structures. -}
parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of
    -- done case
    [] -> maybeToList context

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
        maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

    -- Unordered list case
    ('-' : ' ' : line) : rest ->
        case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
        case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- Code block case
    ('>' : ' ' : line) : rest ->
        case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

    -- Paragraph case
    currentLine : rest ->
        let
            line = trim currentLine
        in
        if line == ""
        then maybe id (:) context (parseLines Nothing rest)
        else
            case context of
            Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
            _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words