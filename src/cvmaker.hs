module Main where

import Text.StringTemplate
import Text.StringTemplate.Helpers

import System.FilePath
import System.IO
import System.Directory

import Paths_cvmaker

workdir = "/Users/wavewave/mac/prog/cvmaker/working"


main = do 
  datadir <- getDataDir
  let templdir = datadir </> "template" 
  putStrLn " cvmaker version 0.0.0.0 " 
  putStrLn $ " templdir = " ++ templdir
  
  putStrLn $ " copying command.tex to " ++ workdir 

  copyFile (templdir </> "command.tex") (workdir </> "command.tex")



