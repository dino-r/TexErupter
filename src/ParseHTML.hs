module ParseHTML (
   Tag(..),
   extractTex,
   parseHTML
) where

import Text.ParserCombinators.Parsec

data Tag = Tex String | Other String deriving (Show, Eq)

parseHTML :: String -> String -> Either ParseError [Tag] 
parseHTML filename input = parse iterateRules filename input
   where iterateRules =
            do first <- rules
               next  <- (eof >> return []) <|> iterateRules
               return (first:next)

rules =
       try tex
   <|> try dollar
   <|> try otherTag
   <|> try other
   <?> "no parsing rules applied"



tex =
   do string "<tex>"
      contents <- manyTill anyChar (try (string "</tex>"))
      return (Tex contents)

dollar =
   do string "<$>"
      contents <- manyTill anyChar (try (string "</$>"))
      -- add invisible bracket to approximately align baseline
      return (Tex ("$\\big." ++ contents ++ "$"))
      
otherTag = 
   do char '<'
      return (Other "<")

other =
   do contents <- many1 (noneOf "<")
      return (Other contents)

-- Convenience function to extract TeX types from Tag list
extractTex :: [Tag] -> [String]
extractTex [] = []
extractTex ((Tex    str):xs) = str:(extractTex xs)
extractTex ( _          :xs) = (extractTex xs)
