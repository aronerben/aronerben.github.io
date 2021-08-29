{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List (
  isSuffixOf,
 )
import Data.Maybe (fromMaybe)
import Data.String
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import qualified GHC.IO.Encoding as E
import Hakyll
import System.Directory
import System.Exit
import System.FilePath
import System.FilePattern ((?==))
import System.Process
import Text.Pandoc.Builder
import Text.Pandoc.Walk

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllMain

hakyllMain :: IO ()
hakyllMain =
  hakyllWith config $ do
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

-- TODO remove dates from post names, add "updated at" field
-- TODO write blog post about anchor

config :: Configuration
config =
  defaultConfiguration
    { deployCommand = "./deploy.sh"
    , watchIgnore = ("_agda/**" ?==)
    }

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions $ walk appendAnchor
  where
    appendAnchor :: Block -> Block
    appendAnchor (Header lvl attr@(id', _, _) txts) =
      Header
        lvl
        attr
        ( txts
            ++ toList
              ( linkWith
                  ("", ["anchor fas fa-xs fa-link"], [])
                  ("#" <> id')
                  ""
                  ""
              )
        )
    appendAnchor x = x

-- Rules
files pattern routing compiler =
  match pattern $
    do
      route routing
      compile compiler

posts :: Pattern -> Routes -> Rules ()
posts pattern routing =
  let newBaseCtx = baseCtx Blog <> boolField "noGreeting" (const True)
   in files pattern routing $
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/base.html" newBaseCtx
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
  updatedTimeField "updated" "%B %e, %Y"
    <> dateField "published" "%B %e, %Y"
    <> defaultContext
  where
    updatedTimeField key format = field key $ \i -> do
      -- TODO CONTINUE HERE, MODIFY/REIMPLEMENT getItemUTC
      time <- getItemUTC defaultTimeLocale $ itemIdentifier i
      return $ formatTime defaultTimeLocale format time

baseCtx :: OverviewPage -> Context String
baseCtx page =
  constField "title" title
    <> boolField (title <> "Class") (const True)
    <> defaultContext
  where
    title = stringifyPage page

data OverviewPage = Index | Blog | About

stringifyPage :: OverviewPage -> String
stringifyPage = \case
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
