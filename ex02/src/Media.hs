module Media (
    User,
    RatingValue(..),
    Rating(..),
    Track(..),
    Album(..),
    MediaLib(..),
    addTrack,
    addAlbum,
    rateTrack,
    albumDurations,
    goodAlbums

) where

import Data.List

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
    duration :: Integer,
    ratings :: [Rating]
    }
    deriving (Show)

instance Eq Track where
    (==) t1 t2 = title t1 == title t2

data Album = Album {
    name :: String,
    tracks :: [Track]
    }
    deriving (Show)

instance Eq Album where
    (==) a1 a2 = name a1 == name a2

data MediaLib = MediaLib {
    albums :: [Album]
    }
    deriving (Show)

-- adds album to a media lib
addAlbum :: Album -> MediaLib -> MediaLib
addAlbum a m = MediaLib (albums m ++ [a])

-- removes album from a media lib
removeAlbum :: Album -> MediaLib -> MediaLib
removeAlbum a m = MediaLib (filter (== a) (albums m))

-- updates Album in a media lib
updateAlbum :: Album -> MediaLib -> MediaLib
updateAlbum a m = addAlbum a (removeAlbum a m)

-- adds a track to a album
addTrack' :: Track -> Album -> Album
addTrack' t a = Album (name a) (tracks a ++ [t])

-- removes a track from a album
removeTrack' :: Track -> Album -> Album
removeTrack' t a = Album (name a) (filter (== t) (tracks a))

-- updates a track in a album
updateTrack' :: Track -> Album -> Album
updateTrack' t a = addTrack' t (removeTrack' t a)

-- rates a track
rateTrack' :: Rating -> Track -> Track
rateTrack' r t =  Track (title t) (artist t) (duration t) (ratings t ++ [r])

-- adds track to a album in a media lib
addTrack :: Track -> Album -> MediaLib -> MediaLib
addTrack t a m = addAlbum (addTrack' t a) (removeAlbum a m)

-- removes a track from a media lib
removeTrack :: Track -> MediaLib -> MediaLib
removeTrack t m = MediaLib (map (\a -> removeTrack' t a) (albums m))

-- get the album of a track
trackAlbum :: Track -> MediaLib -> Album
trackAlbum t m = last (filter (\a -> length (filter (/=t) (tracks a)) == 0) (albums m))

-- updates a track in a media lib
updateTrack :: Track -> MediaLib -> MediaLib
updateTrack t m = addTrack t (trackAlbum t m) (removeTrack t m)

-- adds a rating for a track
rateTrack :: User -> Track -> RatingValue -> MediaLib -> MediaLib
rateTrack u t r m = updateTrack (rateTrack' (Rating u r) t) m

-- calcs the duration of a album
albumDuration :: Album -> Integer
albumDuration a = sum (map duration (tracks a))

-- maps albums and their duration
albumDurations :: MediaLib -> [(Album, Integer)]
albumDurations m = map (\a -> (a,albumDuration a)) (albums m)

goodRatingsByUser :: User -> Track -> Int
goodRatingsByUser u t = length (filter (\r -> value r > Medium) (rbu (ratings t)))
    where rbu rs = filter (\r -> user r == u) (rs)

goodAlbumRatioByUser :: User -> Album -> Double
goodAlbumRatioByUser u a = fromIntegral (length (filter (\t -> (goodRatingsByUser u t) == 0) (tracks a))) / fromIntegral (length (tracks a))

-- return all albums that have at least 50% good tracks
goodAlbums :: User -> MediaLib -> [Album]
goodAlbums u m = filter (\a -> (goodAlbumRatioByUser u a) <= 0.5) (albums m)
