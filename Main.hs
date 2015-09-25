module Main where

import Control.Applicative ((<$>))
import Control.Monad       (filterM, unless, void)
import System.Directory    (doesFileExist, getCurrentDirectory,
                            getDirectoryContents, removeFile)
import System.Environment  (getArgs)
import System.FilePath     (addExtension, splitExtension)

import Conversion
import Runner

main :: IO ()
main = mapM_ (choose . splitExtension) =<< getArgs
  where
  choose (base, ext)
    | ext == ".tex" = makeTex base
    | ext == ".pdf" = makePdf base
    | ext == ".docx"= makeDocx base
    | not $ null ext = error "unknown file type"
    | base == "clean" = cleanupAll
    | otherwise = makePdf base

-- creates .tex from .md
makeTex :: FilePath -> IO ()
makeTex base = do
  let tex = addExtension base "tex"
      md  = addExtension base "md"
  putStrLn $ "Producing: " ++ tex
  writeDoc tex =<< readDoc md

-- creates .docx from .md
makeDocx :: FilePath -> IO ()
makeDocx base = do
  let docx = addExtension base "docx"
      md   = addExtension base "md"
  putStrLn $ "Producing: " ++ docx
  writeWord docx =<< readDoc md


makePdf :: FilePath -> IO ()
makePdf base = do
  let tex = addExtension base "tex"
      pdf = addExtension base "pdf"
  hasTex <- doesFileExist tex
  unless hasTex (void $ makeTex base)
  putStrLn $ "Producing: " ++ pdf
  runLaTeX "lualatex" tex


cleanup :: FilePath -> IO ()
cleanup base = do
  putStrLn $ "Cleaning: " ++ base
  mapM_ removeFile =<< filterM doesFileExist files
  where
    files = map (addExtension base) ["aux","bbl","bcf","blg","fls",
                                    "toc","log","out","dvi","run.xml",
                                    "tex","pdf","rtf", "fdb_latexmk"]

cleanupAll :: IO ()
cleanupAll = do
  files <- getDirectoryContents =<< getCurrentDirectory
  mapM_ cleanup [b | (b, e) <- map splitExtension files, e == ".md"]
  mapM_ removeFile =<< filterM doesFileExist ["missfont.log", "texput.log"]
