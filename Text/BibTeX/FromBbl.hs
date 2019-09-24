{-# LANGUAGE TypeFamilies          #-}

module Text.BibTeX.FromBbl where

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.BibTeX.Entry as BibTeX

import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Void

tqu :: String -> Parsec Void String String
tqu s = whitespace >> char '\\' >> string s

tqu1 :: String -> String -> Parsec Void String (String,String)
tqu1 s f = (,) <$> tqu s <*> braced' (string f)

whitespace :: Parsec Void String String
whitespace = do
   mc <- optional $ oneOf " \n\t%"
   case mc of
    Just c -> do
     comment <- case c of
      '%' -> many $ noneOf "\n"
      _   -> pure ""
     (c:).(comment++) <$> whitespace
    Nothing -> pure ""

braced' :: Parsec Void String a -> Parsec Void String a
braced' p = do
   whitespace
   char '{'
   contents <- go 0
   char '}'
   case runParser (whitespace>>p) "" contents of
     Right r -> pure r
     Left err -> empty
 where go :: Int -> Parsec Void String String
       go n = withRecovery (const $ pure"") $ do
         ws <- whitespace
         c <- noneOf $ if n==0 then ['}'] else []
         (ws++).(c:)<$>case c of
           '{' -> go $ n+1
           '}' -> go $ n-1
           _   -> go   n

braced :: Parsec Void String String
braced = braced' getInput

bblEntry :: Parsec Void String (BibTeX.T)
bblEntry = do
  identifier <- tqu "entry" >> braced
  entryType <- braced
  return $ BibTeX.Cons entryType identifier []

main :: IO ()
main = do
  let fName = "web.bbl"
  fConts <- readFile fName
  let entry = runParser bblEntry fName fConts
  print entry
  return ()
