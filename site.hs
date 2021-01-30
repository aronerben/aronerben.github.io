--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith config $ do
    match ("images/*" .||. "favicon.ico" .||. "CNAME") $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/base.html" postCtx
          >>= relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        let layoutCtx =
              constField "title" "aronwith1a"
                `mappend` constField "blogClass" ""
                `mappend` constField "aboutClass" ""
                `mappend` defaultContext
        getResourceBody >>= applyAsTemplate defaultContext
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
        posts <- recentFirst =<< loadAll "posts/*"
        let blogCtx =
              listField "posts" postCtx (return posts)
                `mappend` defaultContext
        getResourceBody >>= applyAsTemplate blogCtx
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
        getResourceBody >>= loadAndApplyTemplate "templates/base.html" layoutCtx
          >>= relativizeUrls
    match "templates/*" $ compile templateCompiler

config :: Configuration
config = defaultConfiguration {deployCommand = "./deploy.sh"}

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` constField "blogClass" "underline"
    `mappend` constField "aboutClass" ""
    `mappend` defaultContext
