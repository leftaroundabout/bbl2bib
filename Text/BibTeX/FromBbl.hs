{-# LANGUAGE TypeFamilies          #-}

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.BibTeX.Entry as BibTeX

import Data.Void

braced :: Parsec Void String String
braced = do {char '{'; s <- many $ satisfy (\='}'); char '}'; return s}

bblEntry :: Parsec Void String (BibTeX.T)
bblEntry = do
  identifier <- string "\\entry{" >> braced
  entryType <- braced
  return $ BibTeX.Cons entryType identifier []

main :: IO ()
main = do
  f <-
