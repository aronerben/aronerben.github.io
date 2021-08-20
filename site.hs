{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

import Control.Monad
import Data.List (
  isInfixOf,
  isSuffixOf,
 )
import Data.Monoid (mappend)
import Data.String
import Debug.Trace
import qualified GHC.IO.Encoding as E
import Hakyll
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.FilePattern ((?==))
import Data.Functor (($>))

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllMain

hakyllMain :: IO ()
hakyllMain = hakyllWith config $ do
  match ("images/*" .||. "favicon.ico" .||. "CNAME") $ do
    route idRoute
    compile copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/base.html" postCtx
        >>= relativizeUrls
  match (fromString $ agdaOutputDir </> "*.md") $ do
    route $
      setExtension "html"
        `composeRoutes` gsubRoute
          (agdaOutputDir </> "")
          (const "posts")
    compile $ do
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/base.html" postCtx
        >>= relativizeUrls
  match (fromString $ agdaOutputDir </> "*.css") $ do
    route $ gsubRoute (agdaOutputDir </> "") (const "posts")
    compile compressCssCompiler
  match (fromString $ agdaOutputDir </> "*.html") $ do
    route $ gsubRoute (agdaOutputDir </> "") (const "posts")
    compile copyFileCompiler
  match "index.html" $ do
    route idRoute
    compile $ do
      let layoutCtx =
            constField "title" "aronwith1a"
              `mappend` constField "blogClass" ""
              `mappend` constField "aboutClass" ""
              `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate defaultContext
        >>= loadAndApplyTemplate "templates/base.html" layoutCtx
        >>= relativizeUrls
  match "blog.html" $ do
    route idRoute
    compile $ do
      let layoutCtx =
            constField "title" "blog"
              `mappend` constField "blogClass" "underline"
              `mappend` constField "aboutClass" ""
              `mappend` defaultContext
      plainPosts <- loadAll "posts/*"
      agdaPosts <- loadAll $ fromString (agdaOutputDir </> "*.md")
      posts <- recentFirst $ plainPosts ++ agdaPosts
      let blogCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate blogCtx
        >>= loadAndApplyTemplate "templates/base.html" layoutCtx
        >>= relativizeUrls
  match "about.html" $ do
    route idRoute
    compile $ do
      let layoutCtx =
            constField "title" "about"
              `mappend` constField "blogClass" ""
              `mappend` constField "aboutClass" "underline"
              `mappend` defaultContext
      getResourceBody
        >>= loadAndApplyTemplate "templates/base.html" layoutCtx
        >>= relativizeUrls
  match "templates/*" $ compile templateCompiler
  match "agda-posts/*.lagda.md" $ compile $ do
    unsafeCompiler processAgdaPosts
    makeItem ("" :: String)


config :: Configuration
config = defaultConfiguration {deployCommand = "./deploy.sh", watchIgnore = ("_agda/**" ?==)}

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` constField "blogClass" "underline"
    `mappend` constField "aboutClass" ""
    `mappend` defaultContext

-- Agda stuff from https://jesper.sikanda.be/posts/literate-agda.html
agdaCommand :: String
agdaCommand = "agda"

agdaInputDir :: String
agdaInputDir = "agda-posts"

agdaOutputDir :: String
agdaOutputDir = "_agda"

agdaOptions :: String -> [String]
agdaOptions fileName =
  [ "--html"
  , "--html-highlight=auto"
  , "--html-dir=" ++ agdaOutputDir
  , "-i" ++ agdaInputDir
  , agdaInputDir </> fileName
  ]

-- Process a .lagda.md file into markdown by calling Agda
processAgdaPosts :: IO ()
processAgdaPosts = do
  files <- listDirectory agdaInputDir
  let agdaFiles = filter (".lagda.md" `isSuffixOf`) files
  forM_ agdaFiles processAgdaPost

processAgdaPost :: FilePath -> IO ()
processAgdaPost agdaFile = do
  exitCode <-
    readProcessWithExitCode
      agdaCommand
      (agdaOptions agdaFile)
      mempty
  case exitCode of
    (ExitFailure _, err, _) -> do
      putStrLn $ "Failed to process " ++ agdaFile
      putStrLn err
    (ExitSuccess, out, _) -> do
      putStrLn out
