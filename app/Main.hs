{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Clay                           ( render )
import           Control.Monad                  ( (<=<)
                                                , forM_
                                                )
import           Data.String                    ( fromString )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
                                                ( readFile )
import           Data.Text.Lazy                 ( Text
                                                , unpack
                                                )
import           Data.Text.Lazy.IO              ( readFile
                                                , writeFile
                                                )
import           Dhall                          ( FromDhall
                                                , ToDhall
                                                , auto
                                                , input
                                                )
import           Dhall.Deriving                 ( type (<<<)
                                                , CamelCase
                                                , Codec(..)
                                                , DropPrefix
                                                , Field
                                                )
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )
import           Style                          ( index )
import           System.Directory               ( copyFile
                                                , createDirectoryIfMissing
                                                , listDirectory
                                                )
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , a
                                                , body
                                                , docTypeHtml
                                                , footer
                                                , h1
                                                , li
                                                , link
                                                , meta
                                                , nav
                                                , p
                                                , title
                                                , toHtml
                                                , ul
                                                )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes    ( charset
                                                , class_
                                                , content
                                                , href
                                                , media
                                                , name
                                                , onclick
                                                , rel
                                                , style
                                                )
import           Text.Pandoc                    ( PandocMonad
                                                , def
                                                , readMarkdown
                                                , runIOorExplode
                                                , writeHtml5
                                                )
import           Text.Regex.TDFA                ( (=~) )

siteIndex :: Html
siteIndex = docTypeHtml $ do
    H.head $ do
        title "Partial Evaluation"
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1, maximum-scale=1"
        meta ! name "description" ! content "pe200012's personal site"
        link ! rel "stylesheet" ! href "./css/hover.css"
        link ! rel "stylesheet" ! href "./css/index.css"
        link ! rel "shortcut icon" ! href "./favicon.ico"
    body $ H.div ! class_ "container" $ do
        navTemplate
        h1 "Partial Evaluation"
        p "I am an undergraduate student of JNU majoring in CS. I prefer functional programming and am currently learning programming language theory."
        footerTemplate

footerTemplate :: Html
footerTemplate = footer . ul $ do
    li (a ! href "pe1326263755@gmail.com" $ "EMail")
    li (a ! href "https://github.com/pe200012" $ "github.com/pe200012")

navTemplate :: Html
navTemplate = nav . ul $ do
    li (a ! href "./index.html" $ "Home")
    li (a ! href "./blog.html" $ "Blog")
    li (a ! href "./about.html" $ "About")

blogIndex :: [Post] -> Html
blogIndex posts = docTypeHtml $ do
    H.head $ do
        title "Random Writing"
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1, maximum-scale=1"
        meta ! name "description" ! content "pe200012's personal site"
        link ! rel "stylesheet" ! href "./css/hover.css"
        link ! rel "stylesheet" ! href "./css/index.css"
    body $ H.div ! class_ "container" $ do
        navTemplate
        h1 "Blog"
        ul $ forM_ posts $ \p -> li $ do
            H.span ! class_ "postDate" ! style "white-space: nowrap;" $ toHtml $ postDate p
            toHtml @Text " "
            a ! href (fromString (postPath p)) $ toHtml $ postTitle p

blogPost :: Post' -> Html
blogPost post = H.div ! class_ "post" $ do
    h1 $ toHtml (postTitle' post)
    H.div ! class_ "post-content" $ postContent post

convMarkdown :: PandocMonad m => Text -> m Html
convMarkdown = (writeHtml5 def <=< readMarkdown def) . Text.pack . unpack

data Post = Post
    { postTitle :: Text
    , postDate  :: Text
    , postPath  :: FilePath
    }
    deriving Show
    deriving stock Generic
    deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "post")) Post

data Post' = Post'
    { postTitle'  :: Text
    , postDate'   :: Text
    , postContent :: Html
    }

main :: IO ()
main = do
    createDirectoryIfMissing True "./site"
    createDirectoryIfMissing True "./site/css"
    createDirectoryIfMissing True "./site/posts"
    writeFile "./site/index.html" $ renderHtml siteIndex
    writeFile "./site/css/index.css" $ render index
    fs <- listDirectory "./css"
    forM_ fs $ \f -> do
        copyFile ("./css/" ++ f) ("./site/css/" ++ f)
    (config :: [Post]) <- input auto =<< Text.readFile "config.dhall"
    writeFile "./site/blog.html" $ renderHtml
        (blogIndex
            ((\p -> let ((_, _, _, [f]) :: (String, String, String, [String])) = postPath p =~ exceptSuffix in p { postPath = f ++ ".html" }) <$> config)
        )
    forM_ config $ \p -> do
        let (_, _, _, [pa, f]) = postPath p =~ filePathRegex :: (String, String, String, [String])
        fileContent <- runIOorExplode . convMarkdown =<< readFile (postPath p)
        writeFile ("./site/posts/" ++ f ++ ".html") (renderHtml (blogPost (Post' (postTitle p) (postDate p) fileContent)))
  where
    filePathRegex = "(.*)/(.*)\\..*" :: String
    grabFilename  = "/(.*)\\..*"
    exceptSuffix  = "(.*)\\..*" :: String
