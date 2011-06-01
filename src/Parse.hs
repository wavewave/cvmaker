module Parse where

import Control.Applicative
import Control.Monad.Identity

import Text.Parsec

import Type

oneFieldInput :: String -> ParsecT String () Identity String 
oneFieldInput fieldname = do 
  spaces
  string fieldname 
  spaces
  char ':'
  spaces
  str <- many1 (noneOf "\n")
  char '\n'
  return str 

multiLineInput :: String -> [Char] -> ParsecT String () Identity String
multiLineInput fieldname delimiters = do
  spaces
  string fieldname
  spaces 
  char ':' 
  spaces 
  str <- many1 (noneOf delimiters) 
--  try (oneOf delimiters)
  return str 
   

oneGroupFieldInput :: String 
		   -> ParsecT String () Identity a 
		   -> ParsecT String () Identity a
oneGroupFieldInput groupname parser = do 
  spaces
  string groupname
  spaces 
  char '{'
  spaces 
  r <- parser
  spaces 
  char '}'
--  spaces
--  char '\n'
  return r

headerParse :: ParsecT String () Identity HeaderContent
headerParse = 
  Header <$> oneFieldInput "name" 
  	 <*> oneFieldInput "field" 
         <*> oneFieldInput "email"
         <*> oneFieldInput "tel"


educationParse1 :: ParsecT String () Identity EducationContent 
educationParse1 = 
  Education <$> oneFieldInput "period"
             <*> multiLineInput "content" "}"

educationParse :: ParsecT String () Identity EducationContent 
educationParse = oneGroupFieldInput "education" educationParse1 

