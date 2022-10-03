module HsBlog.Convert where

import Prelude hiding (head)
import HsBlog.Env (Env(..))
import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html

convert :: Env -> String -> Markup.Document -> Html.Html
convert env title doc =
  let
    head =
      Html.title_ (eBlogName env <> " - " <> title)
        <> Html.stylesheet_ (eStylesheetPath env)
    article =
      foldMap convertStructure doc
    websiteTitle =
      Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
    body =
      websiteTitle <> article
  in
    Html.html_ head body

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt

    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list

    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Heading 1 heading : article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                <> foldMap convertStructure (take 3 article)
                <> Html.p_ (Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      (Html.title_ "Blog")
      ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )
