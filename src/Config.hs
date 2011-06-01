module Config where

import Control.Applicative 
import Control.Monad.Identity 

import System.FilePath

import Text.Parsec

import Parse

data PathConfig = PathConfig { 
  configWorkDirectory :: FilePath,
  configContentDirectory :: FilePath

} 

parseConfig :: ParsecT String () Identity PathConfig
parseConfig = 
  PathConfig <$> oneFieldInput "contdir" <*> oneFieldInput "workdir"



