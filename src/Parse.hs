module Parse where

-- import Control.Applicative 
import Control.Applicative hiding ((<|>))

import Control.Monad
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
--  oneOf delimiters 
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


profileParse :: ParsecT String () Identity Profile
profileParse = 
   Profile <$> oneFieldInput "birthdate"
           <*> oneFieldInput "birthplace"
           <*> oneFieldInput "citizenship"
           <*> (multiLineInput "address" "|" <* oneOf "|")
           <*> many1 (try educationParse)
           <*> many1 (try experienceParse)
           <*> many1 (try awardParse)

activityParse :: ParsecT String () Identity Activity 
activityParse = 
  Activity <$> many1 (try workshopParse)

educationParse1 :: ParsecT String () Identity EducationContent 
educationParse1 = 
  Education <$> oneFieldInput "period"
            <*> multiLineInput "content" "}"

educationParse :: ParsecT String () Identity EducationContent 
educationParse = oneGroupFieldInput "education" educationParse1 


experienceParse1 :: ParsecT String () Identity ExperienceContent 
experienceParse1 = 
  Experience <$> oneFieldInput "period"
             <*> multiLineInput "title" "}"

experienceParse :: ParsecT String () Identity ExperienceContent 
experienceParse = oneGroupFieldInput "experience" experienceParse1 

awardParse1 :: ParsecT String () Identity AwardContent 
awardParse1 = 
  Award <$> oneFieldInput "date"
        <*> multiLineInput "title" "}"

awardParse :: ParsecT String () Identity AwardContent 
awardParse = oneGroupFieldInput "award" awardParse1 

workshopParse1 :: ParsecT String () Identity Workshop
workshopParse1 = 
  Workshop <$> oneFieldInput "date"
           <*> oneFieldInput "meeting"
           <*> (try (oneFieldInput "event" >>= return . Just) 
                <|> return Nothing) 
           <*> (try (oneFieldInput "seminar" >>= return . Just)
                <|> return Nothing) 

workshopParse :: ParsecT String () Identity Workshop 
workshopParse = oneGroupFieldInput "workshop" workshopParse1 
