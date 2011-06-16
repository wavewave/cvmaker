module Type where

data PageBreak = PageBreak
  deriving Show

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
  activityWorkshops :: [Either PageBreak Workshop],
  activitySeminars  :: [Either PageBreak Seminar]
} deriving Show


data Workshop = Workshop { 
  workshopDate :: String, 
  workshopMeeting :: String, 
  workshopEvent :: Maybe String, 
  workshopSeminar :: Maybe String 
} deriving Show

data Seminar = Seminar { 
  seminarDate  :: String, 
  seminarPlace :: String, 
  seminarEvent :: Maybe String, 
  seminarTitle :: String
} deriving Show

data Publication = Publication { 
  publicationPapers :: [Either PageBreak Paper], 
  publicationProceedings :: [Either PageBreak Proceeding]
} deriving Show


data Paper = Paper { 
  paperAuthors  :: String,
  paperTitle    :: String, 
  paperJournal  :: Maybe String, 
  paperArxiv    :: Maybe String, 
  paperAbstract :: String
} deriving Show

data Proceeding = Proceeding { 
  proceedingAuthors  :: String,
  proceedingTitle    :: String, 
  proceedingConference :: String,
  proceedingJournal  :: String
} deriving Show

