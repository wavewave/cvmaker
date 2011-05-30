module ContentBuild where

import Text.StringTemplate
import Text.StringTemplate.Helpers

import System.FilePath
import Paths_cvmaker

import Type


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
