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


data ExperienceContent = Experience { 
  experiencePeriod  :: String, 
  experienceContent :: String
} deriving Show
