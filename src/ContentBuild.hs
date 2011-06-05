module ContentBuild where

import Text.StringTemplate
import Text.StringTemplate.Helpers

import System.FilePath
import Paths_cvmaker

import Control.Monad.State

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

-- makeWorkshops :: STGroup String -> [Either PageBreak Workshop] -> String
-- makeWorkshops templates = concatMap (makeWorkshop templates)

makeWorkshop :: STGroup String -> Either PageBreak Workshop 
             -> State BlockState String 
makeWorkshop templates e = do 
  case e of 
    Left PageBreak -> do 
      st <- get 
      case st of 
        FirstBlock -> do
          put NextBlock 
          return "\n\\end{resumeblocktwo}\n\n\\pagebreak\n\n\\begin{resumeblock}{}\n\n"
        _ -> do 
          return "\n\\end{resumeblock}\n\n\\pagebreak\n\n\\begin{resumeblock}{}\n\n"
    Right w -> return $ 
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

data BlockState = FirstBlock | NextBlock

makeSeminar :: STGroup String -> Either PageBreak Seminar 
            -> State BlockState String 
makeSeminar templates e = do 
  case e of 
    Left PageBreak -> do 
      st <- get 
      case st of 
        FirstBlock -> do
          put NextBlock 
          return "\n\\end{resumeblocktwo}\n\n\\pagebreak\n\n\\begin{resumeblock}{}\n\n"
        _ -> do 
          return "\n\\end{resumeblock}\n\n\\pagebreak\n\n\\begin{resumeblock}{}\n\n"
    Right s -> return $ 
                 renderTemplateGroup 
                   templates 
                   [ ("date", seminarDate s) 
                   , ("place", seminarPlace s) 
                   , ("event", case (seminarEvent s) of 
                                 Nothing -> "Seminar presented:" 
                                 Just ev -> ev )
                   , ("title", "``" ++ seminarTitle s ++ "''" ) ] 
                   "seminar.tex"


makeActivity :: STGroup String -> Activity -> String 
makeActivity templates p = 
  let concatMapM f lst = liftM concat (mapM f lst) 
      seminarmonad = concatMapM (makeSeminar templates) (activitySeminars p)  
      seminarstr = evalState seminarmonad FirstBlock
      workshopmonad = concatMapM (makeWorkshop templates) (activityWorkshops p)
      workshopstr = evalState workshopmonad FirstBlock
  in renderTemplateGroup 
       templates 
       [ ("workshops", workshopstr )
       , ("seminars" , seminarstr  ) ]
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
