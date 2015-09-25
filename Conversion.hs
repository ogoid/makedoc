module Conversion
    ( readDoc,
      writeDoc,
      writeWord
    ) where

import Control.Applicative ((<$>))
import Control.Monad       (liftM, filterM)
import Data.List           (nub, isInfixOf)
import Data.Maybe          (listToMaybe)
import System.Directory    (doesFileExist)
import System.FilePath     (replaceExtension, takeExtension)

import Text.Pandoc
import Text.Pandoc.Options (CiteMethod(..))
import Text.Pandoc.Templates (getDefaultTemplate)
import Text.Pandoc.Builder (HasMeta, (<>), setMeta, toMetaValue, toList,
                            str, headerWith)
import Text.Pandoc.Shared  (stringify)
import Text.Pandoc.Error (handleError)
import Text.CSL.Pandoc (processCites')

import qualified Data.ByteString.Lazy as B


-- save Pandoc as .tex
writeDoc :: FilePath -> Pandoc -> IO ()
writeDoc path doc = do

  templ <- let isTex file = takeExtension file == ".tex"
               file = listToMaybe $ filter isTex $ queryMeta "template" doc
               notempl = error "no .tex template file specified in document metadata"
           in maybe notempl readFile file

  -- TODO: write a proper detection
  let citeMethod
       | "{biblatex}" `isInfixOf` templ = Biblatex
       | "{natbib}" `isInfixOf` templ = Natbib
       | otherwise = Citeproc

  let options = def {
    writerStandalone = True,
    writerTemplate = templ,
    writerCiteMethod = citeMethod
  }
  writeFile path $ writeLaTeX options doc


-- save Pandoc as .docx, process bibliography with citeproc
writeWord :: FilePath -> Pandoc -> IO ()
writeWord path doc = B.writeFile path =<< writeDocx opts =<< processCites' doc
  where
    opts = def {writerReferenceDocx = listToMaybe templ}
    templ = filter isDocx (queryMeta "template" doc)
    isDocx file = takeExtension file == ".docx"




-- read .md as Pandoc
readDoc :: FilePath -> IO Pandoc
readDoc file = do
  doc <- handleError . readMarkdown def <$> readFile file
  subdocs <- mapM readDoc $ queryMeta "chapters" doc
  let proc x y = mergeDocs x (prependTitle y)
  return $ foldl proc doc subdocs


-- merge two docs concatenating bibliography and bodies
mergeDocs :: Pandoc -> Pandoc -> Pandoc
mergeDocs x y = Pandoc meta' blks'
  where
  blks' = getBlks x <> getBlks y
  meta' = setMeta "bibliography" metabibs (getMeta x)
  metabibs = MetaList $ map (toMetaValue . str) rawbibs
  rawbibs = nub $ queryBib x <> queryBib y
  queryBib = queryMeta "bibliography" . getMeta


-- prepend document title as a chapter header
prependTitle :: Pandoc -> Pandoc
prependTitle (Pandoc meta blks) = Pandoc meta (blk ++ blks)
  where
  blk = toList $ headerWith attr 0 (str title)
  attr | should = ("",[unnum],[])
       | otherwise = nullAttr
  should = lookupMeta unnum meta == Just (MetaBool False)
  unnum = "unnumbered"
  title = stringify $ docTitle meta


--
getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m
getBlks :: Pandoc -> [Block]
getBlks (Pandoc _ b) = b


-- query Pandoc/Meta returning [String]
class HasMeta a => QueryMeta a where
  queryMeta :: String -> a -> [String]

instance QueryMeta Meta where
  queryMeta key meta =
    case lookupMeta key meta of
      Just (MetaList xs) -> map metaValueToString xs
      Just mv            -> [metaValueToString mv]
      Nothing            -> []
    where
    metaValueToString (MetaString s) = s
    metaValueToString (MetaInlines ils) = stringify ils
    metaValueToString (MetaBlocks bs) = stringify bs
    metaValueToString (MetaBool b) = show b
    metaValueToString _ = undefined

instance QueryMeta Pandoc where
  queryMeta key (Pandoc m _) = queryMeta key m
