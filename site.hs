{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

import Control.Monad
import Data.List (
  isSuffixOf,
 )
import Data.Maybe (fromMaybe)
import Data.String
import qualified GHC.IO.Encoding as E
import Hakyll
import System.Directory
import System.Exit
import System.FilePath
import System.FilePattern ((?==))
import System.Process

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllMain

hakyllMain :: IO ()
hakyllMain = hakyllWith config $ do
  files ("images/*" .||. "favicon.ico" .||. "CNAME") idRoute copyFileCompiler
  files "css/*" idRoute compressCssCompiler
  files (agdaPattern "*.css") agdaRoute compressCssCompiler
  files (agdaPattern "*.html") agdaRoute copyFileCompiler
  posts "posts/*" $ setExtension "html"
  posts (agdaPattern "*.md") $ setExtension "html" `composeRoutes` agdaRoute
  overview "index.html" (return defaultContext) Index
  overview
    "blog.html"
    ( do
        plainPosts <- loadAll "posts/*"
        agdaPosts <- loadAll $ fromString (agdaOutputDir </> "*.md")
        allPosts <- recentFirst $ plainPosts ++ agdaPosts
        let descriptionCtx =
              field
                "description"
                ( \item ->
                    fromMaybe ""
                      <$> getMetadataField (itemIdentifier item) "description"
                )
        return $
          listField "posts" postCtx (return allPosts)
            <> defaultContext
            <> descriptionCtx
    )
    Blog
  overview "about.html" (return defaultContext) About
  match "templates/*" $ compile templateCompiler
  match "agda-posts/*.lagda.md" $
    compile $ do
      unsafeCompiler processAgdaPosts
      makeItem ("" :: String)

config :: Configuration
config =
  defaultConfiguration
    { deployCommand = "./deploy.sh"
    , watchIgnore = ("_agda/**" ?==)
    }

-- Rules
files pattern routing compiler =
  match pattern $
    do
      route routing
      compile compiler

posts :: Pattern -> Routes -> Rules ()
posts pattern routing =
  files pattern routing $
    pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/base.html" (baseCtx Blog)
      >>= relativizeUrls

overview :: Pattern -> Compiler (Context String) -> OverviewPage -> Rules ()
overview pattern ctx page =
  files pattern idRoute $ do
    join (applyAsTemplate <$> ctx <*> getResourceBody)
      >>= loadAndApplyTemplate "templates/base.html" (baseCtx page)
      >>= relativizeUrls

-- Contexts
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext

baseCtx :: OverviewPage -> Context String
baseCtx page =
  constField "title" title
    <> constField (title <> "Class") "underline"
    <> defaultContext
  where
    title = stringifyPage page

data OverviewPage = Index | Blog | About

stringifyPage :: OverviewPage -> String
stringifyPage page =
  case page of
    Index -> "aronwith1a"
    Blog -> "blog"
    About -> "about"

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
  filez <- listDirectory agdaInputDir
  let agdaFiles = filter (".lagda.md" `isSuffixOf`) filez
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
    (ExitSuccess, out, _) ->
      putStrLn out

agdaPattern :: IsString a => FilePath -> a
agdaPattern ending = fromString $ agdaOutputDir </> ending

agdaRoute :: Routes
agdaRoute = gsubRoute (agdaOutputDir </> "") (const "posts")
