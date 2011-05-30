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
  templates <- directoryGroup templdir 
  contentstr <- readFile (contdir </> "content.txt") 
  putStrLn "reading content.txt"
  let r = parse headerParse "" contentstr 
  case r of 
    Right h -> do 
      putStrLn (makeHeader templates h) 
    Left  err -> putStrLn (show err)

--  str <- makeCV testcontent 
--  putStrLn str


makeHeader :: STGroup String -> HeaderContent -> String 
makeHeader templates hc = 
  renderTemplateGroup 
    templates
    [ ("name" , headerName  hc) 
    , ("field", headerField hc)
    , ("email", headerEmail hc) 
    , ("tel"  , headerTel   hc) ]
    "header.tex"


oneFieldInput :: String -> ParsecT String () Identity String 
oneFieldInput fieldname = do 
  string (fieldname ++ ":")
  spaces
  str <- many1 (noneOf "\n")
  char '\n'
  return str 

headerParse :: ParsecT String () Identity HeaderContent
headerParse = do
  Header <$> oneFieldInput "name" 
  	 <*> oneFieldInput "field" 
         <*> oneFieldInput "email"
         <*> oneFieldInput "tel"

 
