module ProcessHTML (
   writeProcessedHTML
) where

import ParseHTML (Tag(..))

-- write the modified HTML file to the target directory
writeProcessedHTML :: (String, String) -> String -> String -> [Tag] -> IO ()
writeProcessedHTML opts ifile ofile tags = writeFile ofile (insertSVGReferences opts ifile 1 tags)

-- insert <img ...> tags with the appropriate SVG source and passed options
insertSVGReferences :: (String, String) -> String -> Int -> [Tag] -> String
insertSVGReferences (eqopts, imgopts) filename n [] = []
insertSVGReferences (eqopts, imgopts) filename n ((Other str):xs)  = 
   str ++ (insertSVGReferences (eqopts, imgopts) filename n xs)

insertSVGReferences (eqopts, imgopts) filename n ((Tex str):xs)    = 
   (imgTag imgopts filename str n) 
   ++ (insertSVGReferences (eqopts, imgopts) filename (n+1) xs)

insertSVGReferences (eqopts, imgopts) filename n ((Dollar str):xs) = 
   (imgTag eqopts filename str n) 
   ++ (insertSVGReferences (eqopts, imgopts) filename (n+1) xs)


imgTag :: String -> String -> String -> Int -> String
imgTag opts filename texcode n = 
   "<img " 
    ++ opts 
    ++ " src=\"" ++ filename ++ "." ++ (show n) ++ ".svg\"" 
    ++ " alt=\"" ++ texcode ++ "\">"

