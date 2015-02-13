module Options (
   extractOptions,
   isHelp,
   isVerbose,
   imgOptions,
   eqInlineOptions,
   fontSize,
   outputFile,
   packages,
   showHelp
) where

import System.Console.GetOpt 
import qualified Data.Text as T (pack, unpack, split)

data Flag = Help | Verbose | Package String | FontSize String | Output String 
            | ImgOpts String | EqInlineOpts String deriving (Show,Eq)

options :: [OptDescr Flag]
options =
   [ Option ['h']  ["help"]     (NoArg Help) "Show this help and exit"
   , Option ['v']  ["verbose"]  (NoArg Verbose) "Verbose and keep LaTeX files"
   , Option ['o']  ["output"]   (ReqArg Output "FILE")  "Output FILE"
   , Option ['f']  ["fontsize"] (ReqArg FontSize "FONTSIZE") "change the font size in LaTeX files"
   , Option ['p']  ["package"]  (ReqArg Package "PACKAGES") 
      "Comma separated list of packages to be included in the LaTeX"
   , Option []    ["img"] (ReqArg ImgOpts "STRING") 
      "Attributes for <img>-tags generated from the <tex>-tags"
   , Option []    ["eq-inline"] (ReqArg EqInlineOpts "STRING") 
      "Attributes for <img>-tags generated from the <$>-tags"
   ]

extractOptions :: [String] -> IO ([Flag], [String])
extractOptions argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ showHelp))

-- is the help flag set?
isHelp :: [Flag] -> Bool
isHelp flags = Help `elem` flags

-- is the verbose flag set?
isVerbose :: [Flag] -> Bool
isVerbose flags = Verbose `elem` flags

-- output file name
outputFile :: String -> [Flag] -> String
outputFile defaultname [] = defaultname
outputFile defaultname (flag:flags) = 
   case flag of 
      Output str -> str
      _          -> outputFile defaultname flags

eqInlineOptions :: [Flag] -> String
eqInlineOptions []           = ""
eqInlineOptions (flag:flags) =
   case flag of
      EqInlineOpts str -> str
      _                -> eqInlineOptions flags

imgOptions :: [Flag] -> String
imgOptions []           = ""
imgOptions (flag:flags) =
   case flag of
      ImgOpts str -> str
      _           -> imgOptions flags
   
-- check for fontsize option
fontSize :: String -> [Flag] -> String
fontSize sizedefault []           = sizedefault
fontSize sizedefault (flag:flags) =
   case flag of
      FontSize str -> str
      _            -> fontSize sizedefault flags

-- check for Package Option and return list of Strings
packages :: [Flag] -> [String]
packages [] = []
packages (flag:flags) =
   case flag of
      Package str -> (split str) ++ (packages flags)
      _           -> packages flags
   where split str = ( map T.unpack . T.split (==',') . T.pack) str

-- show help text
showHelp :: String
showHelp = usageInfo "USAGE: texerupter [-hv] [-o FILE] [-f FONTSIZE]\ 
                     \ [-p PACKAGES] [--img=STRING] [--eq-inline=STRING] FILE \n" options

