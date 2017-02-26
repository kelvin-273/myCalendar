import Data.Char
import Data.Hashable
import Data.Time

data User = User { userName :: String
                 , idString :: String
                 , calendar :: [Event]
                --  , friends :: [User]
                 } deriving (Show)

data Event = Event String UTCTime UTCTime deriving (Show)

u1 = initialise "Kelvin"

initialise :: String -> User
initialise name = User {
    userName = name,
    idString = toID name,
    calendar = []
}

toID :: String -> String
toID = show . hash
