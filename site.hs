{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
import           Control.Monad                  ( filterM )
import           Control.Monad.Extra            ( whenM )
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.ByteString.Lazy.Char8     ( ByteString )
import           Data.Monoid                    ( mappend )
import           Hakyll
import           Shelly                         ( cp_r
                                                , shelly
                                                )
import           System.Directory               ( copyFile
                                                , doesDirectoryExist
                                                , getFileSize
                                                , listDirectory
                                                , setCurrentDirectory
                                                )
import           System.Process                 ( callCommand
                                                , readProcess
                                                )
import           Text.Regex.TDFA                ( (=~) )

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "scripts/*" $ do
        route idRoute
        compile copyFileCompiler
    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route idRoute
        compile compressImageCompiler

    match "css/code/css/*" $ do
        route $ gsubRoute "css/code/css" (const "css/code")
        compile compressCssCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" defaultContext >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile
            $   pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            -- >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx = listField "posts" postCtx (return posts) <> constField "title" "Archives" <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                -- >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts) <> defaultContext
            getResourceBody >>= applyAsTemplate indexCtx >>= loadAndApplyTemplate "templates/index.html" indexCtx >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

compressImage :: FilePath -> IO ()
compressImage = whenM . (fmap (> 1000000) . getFileSize) <*> callCommand . ("mogrify -quality 80 -geometry 1920 " ++)

compressImageCompiler :: Compiler (Item ByteString)
compressImageCompiler = do
    (TmpFile tmpFile) <- newTmpFile =<< getUnderlyingExtension
    srcPath           <- getResourceFilePath
    makeItem =<< unsafeCompiler
        (do
            copyFile srcPath tmpFile
            compressImage tmpFile
            BS.readFile tmpFile
        )
