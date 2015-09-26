module Main where

import Control.Applicative ((<$>))
import Control.Monad       (filterM, unless, void, when)
import System.Directory    (doesFileExist, getCurrentDirectory,
                            getDirectoryContents, removeFile)
import System.Environment  (getArgs, getProgName)
import System.FilePath     (addExtension, splitExtension)
import System.Exit         (die)

import Conversion
import Runner


main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) usage
  case splitExtension (head args) of
    ("clean",  "" ) -> cleanupAll
    (base, ".tex" ) -> makeTex base
    (base, ".pdf" ) -> makePdf base
    (base, ".docx") -> makeDocx base
    otherwise -> usage
  where usage = die . unlines . help =<< getProgName
        help prog = header : map (prefix ++) cmds
          where header = "For a file.md source, run:"
                prefix = "  " ++ prog ++ " "
                cmds = [ "file.tex  to generate a LaTeX source"
                       , "file.pdf  to generate a PDF document"
                       , "file.docx to generate a Word document"
                       , "clean     to remove all LaTeX temporary files"
                       ]


-- creates .tex from .md
makeTex :: FilePath -> IO ()
makeTex base = do
  let [tex, md] = addExtension base <$> ["tex", "md"]
  putStrLn $ "Producing: " ++ tex
  writeDoc tex =<< readDoc md

-- creates .docx from .md
makeDocx :: FilePath -> IO ()
makeDocx base = do
  let [docx, md] = addExtension base <$> ["docx", "md"]
  putStrLn $ "Producing: " ++ docx
  writeWord docx =<< readDoc md


makePdf :: FilePath -> IO ()
makePdf base = do
  let [pdf, tex] = addExtension base <$> ["pdf", "tex"]
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
