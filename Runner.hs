module Runner
    ( runLaTeX
    ) where

import Data.Maybe          (isJust)
import Data.List           (isPrefixOf, nub)
import Control.Monad       (void, when)
import System.FilePath     (splitExtension)
import System.Directory    (findExecutable)
import System.Process      (readProcess)


data Command = None | Biber | Bibtex | LaTeX deriving (Eq)

-- runs latex and biber/bibtex until done
runLaTeX :: String -> FilePath -> IO ()
runLaTeX program file = maybe withoutMk withMk =<< findExecutable "latexmk"
  where
  withMk = const $ execLatexMk program base
  base = case splitExtension file of
          (b, ".tex") -> b
          (b, _) -> file
  withoutMk = let go _ None = return ()
                  go 0 _ = error "too many tex executions"
                  go n Biber  = execBiber base >> go (n-1) LaTeX
                  go n Bibtex = execBibtex base >> go (n-1) LaTeX
                  go n LaTeX  = execTex program base >>= go (n-1)
              in go 5 LaTeX

execLatexMk :: String -> FilePath -> IO ()
execLatexMk program file = do
  putStrLn ("Running: latexmk " ++ file)
  void $ readProcess "latexmk"
          [ "-pdflatex=" ++ program
          , "-quiet", "-pdf"
          , file ] ""

execBiber :: FilePath -> IO ()
execBiber file = do
  putStrLn ("Running: biber " ++ file)
  void $ readProcess "biber" ["-q", "-q", file] ""

execBibtex :: FilePath -> IO ()
execBibtex file = do
  putStrLn ("Running: bibtex " ++ file)
  void $ readProcess "bibtex" [file] ""


-- runs latex once
execTex :: String -> FilePath -> IO Command
execTex program file = do
  putStrLn $ "Running: " ++ program ++ " " ++ file
  res <- readProcess program
          ["-halt-on-error",
          "-interaction",
          "nonstopmode",
          file]
          ""
  return $ chooseCmd $ nub $ map toCommand $ lines res
  where
  chooseCmd :: [Command] -> Command
  chooseCmd cmds
    | Biber  `elem` cmds = Biber
    | Bibtex `elem` cmds = Bibtex
    | LaTeX  `elem` cmds = LaTeX
    | otherwise = None
  toCommand :: String -> Command
  toCommand line
    | "Package biblatex Warning: Please (re)run Biber" `isPrefixOf` line = Biber
    | "Package biblatex Warning: Please rerun LaTeX" `isPrefixOf` line = LaTeX
    | "Package rerunfilecheck Warning" `isPrefixOf` line = LaTeX
    -- | "Package natbib Warning" `isPrefixOf` line = Bibtex
    | otherwise = None
