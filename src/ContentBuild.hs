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

makeEducation :: STGroup String -> EducationContent -> String
makeEducation templates ec = 
  renderTemplateGroup
    templates
    [ ("period" , educationPeriod  ec) 
    , ("content", educationContent ec) ] 
    "education.tex"

makeEducations :: STGroup String -> [EducationContent] -> String
makeEducations templates = concatMap (makeEducation templates)

makePersonalProfile :: STGroup String -> Profile -> String 
makePersonalProfile templates p = 
  renderTemplateGroup 
    templates 
    [ ("birthdate"  , profileBirthDate p )
    , ("birthplace" , profileBirthPlace p )
    , ("citizenship", profileCitizenship p ) 
    , ("address"    , profileAddress p )  
    , ("education"  , makeEducations templates (profileEducations p)) ] 
    "profile.tex"



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
