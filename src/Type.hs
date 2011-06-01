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

data Profile = Profile {
  profileEducations :: [EducationContent]
}