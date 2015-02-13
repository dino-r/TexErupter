import System.Environment (getArgs)
import System.Process     (system)
import Control.Monad      (when)
import GHC.IO.Exception   (ExitCode(..))

import Options     (extractOptions, isHelp, isVerbose, eqInlineOptions, imgOptions, fontSize,
                    outputFile, packages, showHelp)
import ParseHTML   (Tag, extractTex, parseHTML)
import ProcessHTML (writeProcessedHTML)
import CompileTex  (processExternal)



main = do
   -- parse command line arguments
   (flags, files) <- getArgs >>= extractOptions 

   -- if help flag is set: exit and print help
   when (isHelp flags) $ error ("Show Help\n\n" ++ showHelp)

   -- if there's no input file, nothing can be done
   when (length files /= 1) $ error ("EXCEPTION -> give exactly one input file\n\n" ++ showHelp)
   let ifile = files !! 0

   -- check if there's a flag that overrides the default fontsize
   let fontsize = fontSize fontsize_default flags

   -- check if there's an output file name given
   let ofile = outputFile (ofile_default ifile) flags 

   {- Check if the verbose flag is set.
    - In verbose mode, we will print the TeX compilation output and don't remove
    - the generated intermediate files -} 
   let verbose = isVerbose flags

   let packagelist = packages flags
   when (verbose) $ do 
      putStrLn $ "Additional packages for LaTeX header:" 
      mapM_ (\arg -> putStrLn $ "   " ++ arg ) packagelist
   -- add packages to the head
   let texHeader = texHeader_default ++ wrapPackagesForTex packagelist
   
   -- read and parse HTML file
   contents <- readFile ifile
   case parseHTML ifile contents of
      -- parse error
      Left err   -> putStrLn $ show err
      -- success: yields tags
      Right tags -> do 
                     -- we need to know the number of external TeX files
                     let texTags = extractTex tags -- Tags with type Tex or Dollar
                         numberOfFiles = length texTags
                     {-
                     - Write the modified HTML file to the target directory.
                     - But first get the Tag Options for inline equations and 
                     - the full TeX environment
                     -}
                     when (verbose) 
                        $ putStrLn ("Process & Write:\n   " ++ ifile ++ " -> " ++ ofile)
                     writeProcessedHTML (eqInlineOptions flags, imgOptions flags) ifile ofile tags
                     
                     -- write and process the externalized tex file to SVGs
                     let wrap = (texHeader, fontSizeTexCommand fontsize)
                     processExternal verbose (ifile ++ ".tex") wrap texTags
                     -- cleanup directory
                     when (not verbose) clean

-- the default LaTeX header
texHeader_default = "\\documentclass[multi, preview, varwidth=\\maxdimen, border={0pt 1.5pt 0pt 0pt}]{standalone}\n\
\\\usepackage{anyfontsize}\n\
\\\usepackage{amsmath}\n\
\\\standaloneenv{page}\n"

-- create \usepackage{..} string for the additional packages
wrapPackagesForTex :: [String] -> String
wrapPackagesForTex pkgs = foldl (\acc x -> acc ++ "\\usepackage{" ++ x ++ "}\n") [] pkgs

-- Default output file name
ofile_default :: String -> String
ofile_default str = "erupted_" ++ str

-- Default font size
fontsize_default :: String
fontsize_default = "14pt"

-- Declare font size
fontSizeTexCommand :: String -> String
fontSizeTexCommand fontsize = "\\fontsize{" ++ fontsize ++ "}{1em}"

-- clean directory of intermediate files
clean :: IO ()
clean = do
   exitcode <- system rmShellCommand
   if exitcode /= ExitSuccess
   then putStrLn $ "Error while removing intermediary files:" ++ rmShellCommand 
   else return ()
   where rmShellCommand = "rm -f *.pdf *.aux *.log *.tex" 
