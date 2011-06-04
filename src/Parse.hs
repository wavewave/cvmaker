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

educationParse :: ParsecT String () Identity EducationContent 
educationParse = oneGroupFieldInput "education" $   
                   Education <$> oneFieldInput "period"
                             <*> multiLineInput "content" "}"


experienceParse :: ParsecT String () Identity ExperienceContent 
experienceParse = oneGroupFieldInput "experience" $
                    Experience <$> oneFieldInput "period"
                               <*> multiLineInput "title" "}"

awardParse :: ParsecT String () Identity AwardContent 
awardParse = oneGroupFieldInput "award" $
               Award <$> oneFieldInput "date"
                     <*> multiLineInput "title" "}"

workshopParse :: ParsecT String () Identity Workshop 
workshopParse = oneGroupFieldInput "workshop" $
                  Workshop <$> oneFieldInput "date"
                           <*> oneFieldInput "meeting"
                           <*> (try (oneFieldInput "event" >>= return . Just) 
                                <|> return Nothing) 
                           <*> (try (oneFieldInput "seminar" >>= return . Just)
                                <|> return Nothing) 

