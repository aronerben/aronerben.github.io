{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_, join)
import Data.List (
  isSuffixOf,
 )
import Data.String (IsString (..))
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeOrError)
import qualified GHC.IO.Encoding as E
import Hakyll
import System.Directory (listDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeFileName, (</>))
import System.FilePattern ((?==))
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Builder (Block (Header), linkWith, toList)
import Text.Pandoc.Options (
  Extension (
    Ext_latex_macros,
    Ext_tex_math_dollars,
    Ext_tex_math_double_backslash
  ),
  HTMLMathMethod (MathJax),
  WriterOptions (writerExtensions, writerHTMLMathMethod),
  extensionsFromList,
 )
import Text.Pandoc.Walk (Walkable (walk))

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllMain

hakyllMain :: IO ()
hakyllMain = do
  -- Initial Agda processing
  -- TODO runs twice when starting
  processAgdaPosts
  hakyllWith config $ do
    files ("images/**" .||. "favicon.ico" .||. "CNAME") idRoute copyFileCompiler
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
          return $
            listField "posts" postCtx (return allPosts)
              <> defaultContext
              <> generalEmptyField "description" return
      )
      Blog
    overview "about.html" (return defaultContext) About
    match "templates/*" $ compile templateCompiler
    match "agda-posts/*.lagda.md" $
      compile $ do
        ident <- getUnderlying
        unsafeCompiler $
          processAgdaPost $
            takeFileName $
              toFilePath ident
        makeItem (mempty :: String)

config :: Configuration
config =
  defaultConfiguration
    { deployCommand = "./deploy.sh"
    , watchIgnore = ("_agda/**" ?==)
    }

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let mathExtensions =
        extensionsFromList
          [ Ext_tex_math_dollars
          , Ext_tex_math_double_backslash
          , Ext_latex_macros
          ]
      writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions =
              writerExtensions
                defaultHakyllWriterOptions
                <> mathExtensions
          , -- Needs to be MathJax despite using KaTeX
            writerHTMLMathMethod = MathJax ""
          }
   in pandocCompilerWithTransform
        defaultHakyllReaderOptions
        writerOptions
        $ walk prependAnchor
  where
    prependAnchor :: Block -> Block
    prependAnchor (Header lvl attr@(id', _, _) txts) =
      Header
        lvl
        attr
        ( toList
            ( linkWith
                (mempty, ["anchor fas fa-xs fa-link"], mempty)
                ("#" <> id')
                mempty
                mempty
            )
            <> txts
        )
    prependAnchor x = x

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
generalEmptyField :: String -> (Maybe String -> Compiler (Maybe String)) -> Context String
generalEmptyField name process =
  field
    name
    ( \item ->
        getMetadataField (itemIdentifier item) name
          >>= process
          >>= maybe (noResult "") return
    )

-- Using custom date context for more control
-- Parse for validation
dateCtx :: String -> Context String
dateCtx name =
  generalEmptyField
    name
    ( return . fmap (format . parse)
    )
  where
    parse :: String -> Day
    parse = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d"
    format = formatTime defaultTimeLocale "%B %e, %Y"

postCtx :: Context String
postCtx =
  dateCtx "updated"
    <> dateCtx "published"
    <> defaultContext

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
