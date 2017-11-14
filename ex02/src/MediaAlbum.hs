module MediaAlbum where

import qualified MediaTrack as T

data Album = Album {
    title :: String,
    tracks :: [T.Track]
    }
    deriving (Show)

instance Eq Album where
    (==) a1 a2 = title a1 == title a2

-- adds a track to a album
addTrack :: T.Track -> Album -> Album
addTrack t a = Album (title a) (tracks a ++ [t])

-- removes a track from a album
removeTrack :: String -> Album -> Album
removeTrack tit a = Album (title a) (filter ((/=) tit . T.title) . tracks $ a)

-- updates a track in a album
updateTrack :: T.Track -> Album -> Album
updateTrack t a = addTrack t (removeTrack (T.title t) a)

-- finds a track in an album if existent
findTrack :: String -> Album -> Maybe T.Track
findTrack tit a = retval . filter ((==) tit . T.title) . tracks $ a
    where retval [] = Nothing
          retval ts = Just (head ts)

-- calcs the duration of a album
duration :: Album -> Int
duration = sum . map T.duration . tracks

-- returns the amount of good rated tracks in the given album
goodRatings :: T.User -> Album -> Int
goodRatings u = length . filter ((/=) 0 . T.goodRatings u) . tracks

-- returns a ratio of good rated tracks in the given album
goodRatingRatio :: T.User -> Album -> Double
goodRatingRatio u a = (fromIntegral . goodRatings u $ a) / (fromIntegral . length . tracks $ a)
