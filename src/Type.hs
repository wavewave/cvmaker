module Type where

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


data EducationContent = Education { 
  educationPeriod  :: String, 
  educationContent :: String
} deriving Show

data ExperienceContent = Experience { 
  experiencePeriod :: String, 
  experienceTitle  :: String
}

data AwardContent = Award { 
  awardPeriod :: String, 
  awardTitle  :: String
}

data Profile = Profile {
  profileBirthDate :: String, 
  profileBirthPlace :: String, 
  profileCitizenship :: String, 
  profileAddress :: String, 
  profileEducations :: [EducationContent], 
  profileExperiences :: [ExperienceContent], 
  profileAwards :: [AwardContent]
}

data Activity = Activity { 
  activityWorkshops :: [Workshop]
} deriving Show

data Workshop = Workshop { 
  workshopDate :: String, 
  workshopMeeting :: String, 
  workshopEvent :: Maybe String, 
  workshopSeminar :: Maybe String 
} deriving Show

