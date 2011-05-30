module Main where

import Control.Monad.Identity
import Control.Applicative

import Text.StringTemplate
import Text.StringTemplate.Helpers

import System.FilePath
import System.IO
import System.Directory

import Text.Parsec

import Paths_cvmaker

contdir = "/Users/wavewave/mac/prog/cvmaker/content"
workdir = "/Users/wavewave/mac/prog/cvmaker/working"


copyFiles :: IO () 
copyFiles = do 
  datadir <- getDataDir
  let templdir = datadir </> "template" 
  putStrLn " cvmaker version 0.0.0.0 " 
  putStrLn $ " templdir = " ++ templdir
  putStrLn $ " copying command.tex to " ++ workdir 
  copyFile (templdir </> "command.tex") (workdir </> "command.tex")

data Content = Content { 
  header :: String, 
  personalProfile :: String,
  professionalActivity :: String, 
  publications :: String, 
  proceedings :: String
}

data HeaderContent = Header { 
  headerName  :: String,
  headerField :: String, 
  headerEmail :: String,
  headerTel   :: String
} deriving Show

testcontent = Content { 
  header  = "",  
  personalProfile = "", 
  professionalActivity = "", 
  publications = "",  
  proceedings = ""
} 

makeCV :: Content -> IO String 
makeCV c = do 
  templdir <- return . (</> "template" ) =<< getDataDir 
  templates <- directoryGroup templdir 
  let str = renderTemplateGroup templates 
                      [ ("Header"              , header               c) 
                      , ("PersonalProfile"     , personalProfile      c)
                      , ("ProfessionalActivity", professionalActivity c)
                      , ("Publications"        , publications         c)
		      , ("Proceedings"         , proceedings          c) ]
                      "cv.tex"
  return str

main = do 
  copyFiles
  templdir <- return . (</> "template" ) =<< getDataDir 
--  templates <- directoryGroup templdir 
  contentstr <- readFile (contdir </> "content.txt") 
  putStrLn "reading content.txt"
  let -- r = parse headerParse "" contentstr 
     r = parse tempparse "" contentstr
  case r of 
    Right h -> do 
      putStrLn (show h) -- (makeHeader templates h) 
    Left  err -> putStrLn (show err)

--  str <- makeCV testcontent 
--  putStrLn str

tempparse = do 
  headerParse
  experienceParse


makeHeader :: STGroup String -> HeaderContent -> String 
makeHeader templates hc = 
  renderTemplateGroup 
    templates
    [ ("name" , headerName  hc) 
    , ("field", headerField hc)
    , ("email", headerEmail hc) 
    , ("tel"  , headerTel   hc) ]
    "header.tex"

{-
makePersonalProfile :: STGroup String -> ProfileContent -> String 
makePersonalProfile templates pc = 
  renderTemplateGroup 
    templates 
    [ ("educationlist", educationStr (educationList pc)) ] 
    "education.tex"
-}



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


data ExperienceContent = Experience { 
  experiencePeriod  :: String, 
  experienceContent :: String
} deriving Show


experienceParse1 :: ParsecT String () Identity ExperienceContent 
experienceParse1 = 
  Experience <$> oneFieldInput "period"
             <*> multiLineInput "content" "}"

experienceParse :: ParsecT String () Identity ExperienceContent 
experienceParse = oneGroupFieldInput "education" experienceParse1 