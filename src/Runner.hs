module Runner
    ( runLaTeX
    ) where

import Data.Maybe          (isJust)
import Data.List           (isPrefixOf, nub)
import Control.Monad       (void, when)
import System.FilePath     (splitExtension)
import System.Directory    (findExecutable)
import System.Process      (readProcess, callProcess)


data Command = None | LaTeX | LaTeXMk | Biber | Bibtex deriving (Eq, Show)

runLaTeX :: String -> String -> IO ()
runLaTeX program file = go 0 . choose =<< findExecutable "latexmk"
  where
    choose = maybe LaTeX (const LaTeXMk)
    go _ None = return ()
    go 5 _    = error "too many tex executions"
    go n cmd  = do
      let base = case splitExtension file of
                 (b, ".tex") -> b
                 (b, _) -> file
      putStrLn $ "Running: " ++ show cmd ++ " " ++ base
      run cmd program base >>= go (n+1)

run :: Command -> String -> String -> IO Command
run Biber   _       base = callProcess "biber" ["-q", "-q", base] >> return LaTeX
run Bibtex  _       base = callProcess "bibtex" [base] >> return LaTeX
run LaTeXMk program base = callProcess "latexmk" opts >> return None
              where opts = [ "-pdflatex=" ++ program, "-quiet", "-pdf", base ]
run LaTeX   program base = processLog <$> readProcess program opts ""
  where
  opts = [ "-halt-on-error", "-interaction", "nonstopmode", base ]
  processLog = choose . nub . map toCommand . lines
  choose cmds
    | Biber  `elem` cmds = Biber
    | Bibtex `elem` cmds = Bibtex
    | LaTeX  `elem` cmds = LaTeX
    | otherwise = None
  toCommand line
    | "Package biblatex Warning: Please (re)run Biber" `isPrefixOf` line = Biber
    | "Package biblatex Warning: Please rerun LaTeX" `isPrefixOf` line = LaTeX
    | "Package rerunfilecheck Warning" `isPrefixOf` line = LaTeX
    -- | "Package natbib Warning" `isPrefixOf` line = Bibtex
    | otherwise = None
