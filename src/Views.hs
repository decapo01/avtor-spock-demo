{-# LANGUAGE OverloadedStrings #-}
module Views where

import           ClassyPrelude
import           Lucid.Base
import           Lucid.Html5
import           Data.Text
import qualified Data.Text as T


signInFormView :: Html ()
signInFormView = do
  form_ [method_ "post"] $ do
    div_ $ do
      label_ "Email"
      input_ [type_ "text", name_ "email"]
    div_ $ do
      label_ "Password"
      input_ [type_ "password", name_ "password"]
    div_ $ do
      input_ [type_ "Submit"]


signUpFormView :: Html ()
signUpFormView = do
  form_ [method_ "post"] $ do
    div_ $ do
      label_ "Email"
      input_ [type_ "text", name_ "email"]
    div_ $ do
      label_ "Password"
      input_ [type_ "password", name_ "password"]
    div_ $ do
      label_ "Confirm Password"
      input_ [type_ "password", name_ "confirm"]
    div_ $ do
      input_ [type_ "submit"]