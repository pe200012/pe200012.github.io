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
import           Data.List                      ( foldl' )
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
import           Style                          ( index
                                                , postCss
                                                )
import           System.Directory               ( copyFile
                                                , createDirectoryIfMissing
                                                , listDirectory
                                                , removeFile
                                                , renameFile
                                                )
import           System.Process                 ( callProcess )
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
                                                , script
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
                                                , onload
                                                , rel
                                                , src
                                                , style
                                                )
import           Text.Pandoc                    ( HTMLMathMethod(..)
                                                , PandocMonad
                                                , ReaderOptions(readerColumns, readerExtensions)
                                                , WrapOption(WrapAuto, WrapNone)
                                                , WriterOptions(writerHTMLMathMethod, writerReferenceLinks, writerWrapText)
                                                , def
                                                , getAllExtensions
                                                , githubMarkdownExtensions
                                                , readMarkdown
                                                , runIOorExplode
                                                , strictExtensions
                                                , writeHtml5
                                                , writeMarkdown
                                                )
import           Text.Regex.TDFA                ( (=~)
                                                , AllMatches(AllMatches, getAllMatches)
                                                , Regex
                                                , RegexContext(match)
                                                )

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
        navTemplate "./"
        h1 "Partial Evaluation"
        p "I am an undergraduate student of JNU majoring in CS. I prefer functional programming and am currently learning programming language theory."
        footerTemplate

footerTemplate :: Html
footerTemplate = footer . ul $ do
    li (a ! href "pe1326263755@gmail.com" $ "EMail")
    li (a ! href "https://github.com/pe200012" $ "github.com/pe200012")

navTemplate :: FilePath -> Html
navTemplate pa = nav . ul $ do
    li (a ! href (fromString (pa ++ "index.html")) $ "Home")
    li (a ! href (fromString (pa ++ "./blog.html")) $ "Blog")
    li (a ! href (fromString (pa ++ "./about.html")) $ "About")

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
        navTemplate "./"
        h1 "Blog"
        ul $ forM_ posts $ \p -> li $ do
            H.span ! class_ "postDate" ! style "white-space: nowrap;" $ toHtml $ postDate p
            toHtml @Text " "
            a ! href (fromString (postPath p)) $ toHtml $ postTitle p

blogPost :: Post' -> Html
blogPost post = docTypeHtml $ do
    H.head $ do
        title $ toHtml (postTitle' post)
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1, maximum-scale=1"
        meta ! name "description" ! content "pe200012's personal site"
        link ! rel "stylesheet" ! href "../css/hover.css"
        link ! rel "stylesheet" ! href "../css/post.css"
        link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.css"
        script ! src "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/katex.min.js" $ pure ()
        script ! src "https://cdn.jsdelivr.net/npm/katex@0.13.13/dist/contrib/auto-render.min.js" ! onload "renderMathInElement(document.body);" $ pure ()
        script
            "document.addEventListener(\"DOMContentLoaded\", function () {\
                \ var mathElements = document.getElementsByClassName(\"math\");\
                \ var macros = [];\
                \ for (var i = 0; i < mathElements.length; i++) {\
                \  var texText = mathElements[i].firstChild;\
                \  if (mathElements[i].tagName == \"SPAN\") {\
                \   katex.render(texText.data, mathElements[i], {\
                \    displayMode: mathElements[i].classList.contains('display'),\
                \    throwOnError: false,\
                \    macros: macros,\
                \    fleqn: false\
                \   });\
                \}}});"
    body $ do
        navTemplate "../"
        h1 $ toHtml (postTitle' post)
        H.div ! class_ "post-content" $ postContent post

convMarkdown :: PandocMonad m => Text -> m Html
convMarkdown =
    (   writeHtml5 (def { writerReferenceLinks = True, writerHTMLMathMethod = KaTeX "", writerWrapText = WrapNone })
        <=< readMarkdown (def { readerExtensions = getAllExtensions "markdown", readerColumns = 0 })
        )
        . Text.pack
        . unpack

-- >>> replaceAll "(?<=c)aa" (const "bb") "caaaa"
-- Explict error in module Text.Regex.TDFA.String : Text.Regex.TDFA.String died: parseRegex for Text.Regex.TDFA.String failed:"(?<=c)aa" (line 1, column 2):
-- unexpected "?"
-- expecting empty () or anchor ^ or $ or an atom

replaceAll :: String -> (String -> String) -> String -> String
replaceAll re f s = start end
  where
    (_, end, start) = foldl' go (0, s, id) $ getAllMatches (s =~ re :: AllMatches [] (Int, Int))
    go (ind, read, write) (off, len) =
        let (skip   , start    ) = splitAt (off - ind) read
            (matched, remaining) = splitAt len start
        in  (off + len, remaining, write . (skip ++) . (f matched ++))

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
    writeFile "./site/css/post.css" $ render postCss
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
        copyFile (postPath p) ("./site/posts/" ++ f ++ ".md")
        -- callProcess "sed" ["-i", "s/\\$\\$\\([^$]*\\)\\$\\$/<span class=\"math display\">\\1<\\/span>/g", "./site/posts/" ++ f ++ ".md"]
        -- callProcess "sed" ["-i", "s/\\$\\([^$]*\\)\\$/<span class=\"math inline\">\\1<\\/span>/g", "./site/posts/" ++ f ++ ".md"]
        callProcess "sed" ["-i", "s/#### *\\(.*\\)/<h4 id=\"\\1\">\\1<\\/h4>/g", "./site/posts/" ++ f ++ ".md"]
        callProcess "sed" ["-i", "s/### *\\(.*\\)/<h3 id=\"\\1\">\\1<\\/h3>/g", "./site/posts/" ++ f ++ ".md"]
        callProcess "sed" ["-i", "s/## *\\(.*\\)/<h2 id=\"\\1\">\\1<\\/h2>/g", "./site/posts/" ++ f ++ ".md"]
        callProcess "sed" ["-i", "s/# *\\(.*\\)/<h1 id=\"\\1\">\\1<\\/h1>/g", "./site/posts/" ++ f ++ ".md"]
        fileContent <- runIOorExplode . convMarkdown =<< readFile ("./site/posts/" ++ f ++ ".md")
        removeFile ("./site/posts/" ++ f ++ ".md")
        -- callProcess "pandoc" ["-s", "--katex", postPath p, "-o", pa ++ f ++ ".html", "-c", "../css/post.css"]
        -- renameFile (pa ++ f ++ ".html") ("./site/posts/" ++ f ++ ".html")
        writeFile ("./site/posts/" ++ f ++ ".html") (renderHtml (blogPost (Post' (postTitle p) (postDate p) fileContent)))
  where
    filePathRegex = "(.*)/(.*)\\..*" :: String
    grabFilename  = "/(.*)\\..*"
    exceptSuffix  = "(.*)\\..*" :: String
