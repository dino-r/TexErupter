module ProcessHTML (
   writeProcessedHTML
) where

import ParseHTML (Tag(..))

-- write the modified HTML file to the target directory
writeProcessedHTML :: String -> String -> [Tag] -> IO ()
writeProcessedHTML ifile ofile tags = writeFile ofile (insertSVGReferences ifile 1 tags)

-- insert <img ...> tags with the appropriate SVG source
insertSVGReferences :: String -> Integer -> [Tag] -> String
insertSVGReferences filename n [] = []
insertSVGReferences filename n ((Other str):xs) = 
   str ++ (insertSVGReferences filename n xs)
insertSVGReferences filename n ((Tex str):xs)   = 
   (img n str) ++ (insertSVGReferences filename (n+1) xs)
   where img n str = "<img src=\"" ++ filename ++ "." ++ (show n) 
                     ++ ".svg\" alt=\"" ++ str ++ "\">"
