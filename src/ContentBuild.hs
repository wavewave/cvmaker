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

makeExperience :: STGroup String -> ExperienceContent -> String
makeExperience templates ec = 
  renderTemplateGroup
    templates
    [ ("period" , experiencePeriod  ec) 
    , ("title"  , experienceTitle   ec) ] 
    "experience.tex"

makeExperiences :: STGroup String -> [ExperienceContent] -> String
makeExperiences templates = concatMap (makeExperience templates)


makeAward :: STGroup String -> AwardContent -> String
makeAward templates ec = 
  renderTemplateGroup
    templates
    [ ("period" , awardPeriod  ec) 
    , ("title"  , awardTitle   ec) ] 
    "award.tex"

makeAwards :: STGroup String -> [AwardContent] -> String
makeAwards templates = concatMap (makeAward templates)


makePersonalProfile :: STGroup String -> Profile -> String 
makePersonalProfile templates p = 
  renderTemplateGroup 
    templates 
    [ ("birthdate"  , profileBirthDate p )
    , ("birthplace" , profileBirthPlace p )
    , ("citizenship", profileCitizenship p ) 
    , ("address"    , profileAddress p )  
    , ("education"  , makeEducations templates (profileEducations p)) 
    , ("experiences", makeExperiences templates (profileExperiences p)) 
    , ("awards"     , makeAwards templates (profileAwards p)) ]
    "profile.tex"

makeWorkshops :: STGroup String -> [Workshop] -> String
makeWorkshops templates = concatMap (makeWorkshop templates)

makeWorkshop :: STGroup String -> Workshop -> String 
makeWorkshop templates w = 
  renderTemplateGroup 
    templates 
    [ ("date", workshopDate w ) 
    , ("meeting", workshopMeeting w) 
    , ("event", case (workshopEvent w) of 
                  Nothing -> "" 
                  Just ev -> ev )
    , ("seminar", case (workshopSeminar w) of 
                    Nothing -> "" 
                    Just sem -> "``" ++ sem ++ "''" ) ] 
    "workshop.tex"


makeActivity :: STGroup String -> Activity -> String 
makeActivity templates p = 
  renderTemplateGroup 
    templates 
    [ ("workshops", makeWorkshops templates (activityWorkshops p)) ]
    "activity.tex"



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
