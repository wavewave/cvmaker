module Main where

import Control.Monad.Identity
import Control.Applicative

import Text.StringTemplate
import Text.StringTemplate.Helpers

import System.FilePath
import System.IO
import System.Directory
import System.Environment

import Text.Parsec

import Paths_cvmaker

import Type
import Parse
import ContentBuild
import Config

getContentDirectory :: IO FilePath
getContentDirectory = do
  homedir <- getEnv "HOME" 
  str <- readFile (homedir </> ".cvconfig")
  let r = parse parseConfig "" str 
  case r of
    Right (PathConfig contdir _) -> return contdir 
    Left  err -> error (show err)


getWorkDirectory :: IO FilePath
getWorkDirectory = do
  homedir <- getEnv "HOME"
  str <- readFile (homedir </> ".cvconfig")
  let r = parse parseConfig "" str 
  case r of
    Right (PathConfig _ workdir) -> return workdir 
    Left  err -> error (show err) 
    

copyFiles :: IO () 
copyFiles = do 
  datadir <- getDataDir
  contdir <- getContentDirectory 
  workdir <- getWorkDirectory

  let templdir = datadir </> "template" 
  putStrLn " cvmaker version 0.0.0.0 " 
  putStrLn $ " templdir = " ++ templdir
  putStrLn $ " copying command.tex to " ++ workdir 
  copyFile (templdir </> "command.tex") (workdir </> "command.tex")
  copyFile (templdir </> "simplemargins.sty") (workdir </> "simplemargins.sty")


testcontent = Content { 
  header  = "",  
  personalProfile = "", 
  professionalActivity = "", 
  publications = "",  
  proceedings = ""
} 


main = do 
  contdir <- getContentDirectory 
  workdir <- getWorkDirectory
  copyFiles
  templdir <- return . (</> "template" ) =<< getDataDir 
  templates <- directoryGroup templdir 
  contentstr <- readFile (contdir </> "content.txt") 
  putStrLn "reading content.txt"
  let -- r = parse headerParse "" contentstr 
     r = parse mainparse "" contentstr
  case r of 
    Right (h,p) -> do 
      let c = Content { header = makeHeader templates h
                      , personalProfile = makePersonalProfile templates p 
                      , professionalActivity = "" 
                      , publications = ""
                      , proceedings = ""
                      }
      makeCV c >>= writeFile (workdir </> "cv.tex") 
      
      


--      putStrLn $ makeHeader templates h 
---                 ++ makePersonalProfile templates p 

--(show h) -- (makeHeader templates h) 
    Left  err -> putStrLn (show err)

--  str <- makeCV testcontent 
--  putStrLn str

mainparse = do 
  h <- headerParse
  p <- profileParse
  return (h,p)