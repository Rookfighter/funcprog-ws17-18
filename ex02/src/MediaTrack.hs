module MediaTrack where

type User = String

data RatingValue = VeryBad | Bad | Medium | Good | VeryGood
    deriving (Show, Eq, Ord)

data Rating = Rating {
    user :: User,
    value :: RatingValue
    }
    deriving (Show)

data Track = Track {
    title :: String,
    artist :: String,
    duration :: Int,
    ratings :: [Rating]
    }
    deriving (Show)

instance Eq Track where
    (==) t1 t2 = title t1 == title t2


-- rates a track
rateTrack :: Rating -> Track -> Track
rateTrack r t =  Track (title t) (artist t) (duration t) (ratings t ++ [r])

-- returns the amount of good ratings of a user
goodRatings :: User -> Track -> Int
goodRatings u = length . filter ((<) Medium . value) . filter ((==) u . user) . ratings
