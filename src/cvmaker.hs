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

import Type
import Parse
import ContentBuild



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




testcontent = Content { 
  header  = "",  
  personalProfile = "", 
  professionalActivity = "", 
  publications = "",  
  proceedings = ""
} 


main = do 
  copyFiles
  templdir <- return . (</> "template" ) =<< getDataDir 
  templates <- directoryGroup templdir 
  contentstr <- readFile (contdir </> "content.txt") 
  putStrLn "reading content.txt"
  let -- r = parse headerParse "" contentstr 
     r = parse mainparse "" contentstr
  case r of 
    Right (h,p) -> do 
      putStrLn $ makeHeader templates h 
                 ++ makePersonalProfile templates p 

--(show h) -- (makeHeader templates h) 
    Left  err -> putStrLn (show err)

--  str <- makeCV testcontent 
--  putStrLn str

mainparse = do 
  h  <- headerParse
  es <- many1 (try educationParse)

  return (h,Profile es)