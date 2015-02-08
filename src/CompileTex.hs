module CompileTex (
   processExternal
) where

import System.Process (system)
import GHC.IO.Exception (ExitCode(..))
import Data.List (intersperse)
import Control.Monad (when)

type VerboseFlag   = Bool
type FileName      = String
type TexHeader     = String
type TexCommands   = String
type TexWrap       = (TexHeader, TexCommands)
type TexContent    = String
type TexDocument   = String
type Executable    = String
type NumberOfFiles = Int


processExternal :: VerboseFlag -> FileName -> TexWrap -> [TexContent] -> IO ()
processExternal verbose filename wrap contents = 
   do when (verbose) 
         $ putStrLn ("Wrap extracted LaTeX code into a proper Document:\n   "
                     ++ filename)
      externalize filename wrap contents

      -- in verbose mode, we want pdflatex to print its progress 
      -- and to halt if there are issues with the LaTeX file
      when (verbose) $ putStrLn ("LaTeX Compilation:\n   "
                                 ++ texShellCommand verbose filename)
      when (verbose) $ putStrLn "\n--------------------------------------------------\n"
      latexexitcode <- system $ (texShellCommand verbose) filename
      when (verbose) $ putStrLn "\n--------------------------------------------------\n"
      if latexexitcode /= ExitSuccess
      then putStrLn pdferror
      else do let pdffile = replaceSuffix "pdf" filename
              when (verbose) $ putStrLn ("Convert to SVG images. " 
                                         ++ "Run once for each page number <n>:\n   "
                                         ++ "pdf2svg " ++ pdffile ++ " " 
                                         ++ replaceSuffix "<n>.svg" filename ++ " "
                                         ++ "<n>")
              svgexitcode <- pdf2svg (length contents) (replaceSuffix "pdf" filename)
              if svgexitcode /= ExitSuccess
              then putStrLn svgerror
              else return ()
   where pdferror = "Error in pdflatex: Probably a syntax error.\n   " ++
                    "Run in verbose mode [-v] for the error report of pdflatex"
         svgerror = "Error in pdf2svg"

replaceSuffix :: String -> FileName -> FileName
replaceSuffix suffix filename = ((reverse . dropWhile (/= '.') . reverse) filename) ++ suffix

-- the pdflatex executable for the shell
texShellCommand :: VerboseFlag -> FileName -> String
texShellCommand verbose ifile =
   if (verbose) then
      "pdflatex " ++ ifile
   else
      "pdflatex --interaction=nonstopmode " ++ ifile ++ " >/dev/null"
      

-- wraps LaTex into a proper document and writes it to a file 
externalize :: FilePath -> TexWrap -> [TexContent] -> IO ()
externalize filepath wrap contents = writeFile filepath (wrapTex wrap contents)

-- wrap extracted LaTex into a proper LaTex document
wrapTex :: TexWrap -> [TexContent] -> TexDocument
wrapTex (header, commands) contents = header ++ begin ++ commands ++ pagination ++ end
   where pagination = concat  
                      $  ["\n\\begin{page}\n"]
                      ++ (intersperse "\n\\end{page}\n\\begin{page}\n" contents)
                      ++ ["\n\\end{page}\n"]
         begin = "\n\\begin{document}\n"
         end   = "\n\\end{document}\n"

-- compile LaTex
pdfLatex :: (Executable -> String) -> FilePath -> IO ExitCode
pdfLatex exe filepath = system $ exe filepath

-- convert PDF to SVG
pdf2svg :: NumberOfFiles -> FilePath -> IO ExitCode
pdf2svg i pdfFile = if i > 0 
                    then do transform i 
                            pdf2svg (i - 1) pdfFile 
                    else return ExitSuccess
   where transform i = system $ "pdf2svg " ++ pdfFile ++ " " ++ (svgFile i) ++ " " ++ (show i)
         svgFile i = replaceSuffix ((show i) ++ ".svg") pdfFile

