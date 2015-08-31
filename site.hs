--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Set as S
import           Data.Map as M
import           Data.List hiding (span)
import           Data.Ord
import           Text.Pandoc.Options


-- how many tags to display on the front page
tagCount = 10

-------------------------------------------------------------------------------
pandocMathCompiler =
  let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                       Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = Prelude.foldr S.insert defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions {
        writerExtensions = newExtensions,
        writerHTMLMathMethod = MathJax ""
        }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "site.hs" $ do
        route idRoute
        compile copyFileCompiler

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    let myContext = listField "globalTags" tagCtx tagsAsItems `mappend`
                    defaultContext

        tagsAsItems = mapM tagAsItem
                      (--take tag count
                       reverse .
                       sortBy (comparing (length . snd)) .
                       tagsMap $ tags)
        tagAsItem (tag, posts) = makeItem (tag, posts)

        tagCtx = field "tag" (return . fst . itemBody) `mappend`
                 field "postCount" (return . show . length . snd . itemBody)

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
          -- underlying <- getUnderlying
          -- myTags <- getTagsQuoted underlying
            >>= loadAndApplyTemplate "templates/post.html"    (postsCtxWithTags tags)
            -- >>= loadAndApplyTemplate "templates/content.html" (postsCtxWithTags tags)
            -- >>= loadAndApplyTemplate "templates/default.html" (postsCtxWithTags tags)
            -- >>= loadAndApplyTemplate "templates/post.html"    (postsCtx `mappend` myTagsCtx myTags)
            >>= loadAndApplyTemplate "templates/content.html" (postsCtxWithTags tags `mappend` myContext)
            >>= loadAndApplyTemplate "templates/default.html" (postsCtxWithTags tags `mappend` myContext)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title `mappend` listField "posts" postsCtx (return posts) `mappend` myContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


    match "about.md" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            -- >>= loadAndApplyTemplate "templates/post.html"    postsCtx
            -- >>= loadAndApplyTemplate "templates/content.html" postsCtx
            -- >>= loadAndApplyTemplate "templates/default.html" postsCtx
            >>= loadAndApplyTemplate "templates/post.html"    myContext
            >>= loadAndApplyTemplate "templates/content.html" myContext
            >>= loadAndApplyTemplate "templates/default.html" myContext
            >>= relativizeUrls

    match "ancient/*" $ do
        -- route $ setExtension "html"
        -- compile $ do
        --   underlying <- getUnderlying
        --   myTags <- getTagsQuoted underlying
        --   pandocMathCompiler
        --     -- >>= loadAndApplyTemplate "templates/post.html"    (postsCtxWithTags tags)
        --     -- >>= loadAndApplyTemplate "templates/content.html" (postsCtxWithTags tags)
        --     -- >>= loadAndApplyTemplate "templates/default.html" (postsCtxWithTags tags)
        --     >>= loadAndApplyTemplate "templates/post.html"    (postsCtx `mappend` myTagsCtx myTags)
        --     >>= relativizeUrls
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postsCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/content.html" (postsCtxWithTags tags `mappend` myContext)
            >>= loadAndApplyTemplate "templates/default.html" (postsCtxWithTags tags `mappend` myContext)
            -- >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postsCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    myContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/content.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postsCtx (return posts) `mappend`
                    -- tagCloudField "tags" 100 100 tags `mappend`
                    constField "title" "Home"                `mappend`
                    myContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/content_home.html" indexCtx
                >>= loadAndApplyTemplate "templates/default_home.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postsCtx :: Context String
postsCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
postsCtxWithTags :: Tags -> Context String
postsCtxWithTags tags = tagsField "tags" tags `mappend` postsCtx

-- myTagsCtx :: [String] -> Context String
-- myTagsCtx tags = listField "myTags" simpleTagCtx (mapM makeItem tags)
--                  where simpleTagCtx = field "tag" (return . itemBody)


-- -- obtain tags from a page in a almost default way: parse them from metadada field
-- getTagsQuoted :: MonadMetadata m=> Identifier -> m [String]
-- getTagsQuoted identifier = do
--   metadata <- getMetadata identifier
--   return $ maybe [] (Prelude.map trim . splitAll "," . unQuote) $ M.lookup "tags" metadata

-- -- takes a string possibly wrapped in quotes and remove them
-- unQuote :: String -> String
-- unQuote s = let s' = if Prelude.head s == '"' then Prelude.tail s else s
--                 s''= if last s' == '"' then init s' else s'
--             in s''
