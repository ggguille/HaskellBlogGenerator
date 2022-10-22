module HsBlog.Html
  ( -- * HTML EDSL
  Html
  , html_

  -- ** Combinators used to construct the @\<head\>@ section
  , Head
  , title_
  , stylesheet_
  , meta_

  -- ** Combinators used to construct the @\<body\>@ section
  , Structure
  , p_
  , h_
  , ul_
  , ol_
  , code_

  -- ** Combinators used to construct content inside structures
  , Content
  , txt_
  , img_
  , link_
  , b_
  , i_

  -- ** Render HTML to String
  , render
  )
  where

  import Prelude hiding (head)
  import HsBlog.Html.Internal