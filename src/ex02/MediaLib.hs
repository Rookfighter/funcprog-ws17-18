-- MediaLib.hs
--
--     Author: Fabian Meyer
-- Created on: 26 Oct 2017

module MediaLib where

import Data.List
import qualified MediaTrack as T
import qualified MediaAlbum as A
import qualified Tracks as TL

data MediaLib = MediaLib {
    albums :: [A.Album]
    }
    deriving (Show)

{---------------
Album operations
-----------------}

-- adds album to a media lib
addAlbum :: A.Album -> MediaLib -> MediaLib
addAlbum a m = MediaLib (albums m ++ [a])

-- removes album from a media lib
removeAlbum :: A.Album -> MediaLib -> MediaLib
removeAlbum a m = MediaLib (filter (/= a) (albums m))

-- updates Album in a media lib
updateAlbum :: A.Album -> MediaLib -> MediaLib
updateAlbum a m = addAlbum a (removeAlbum a m)

-- get Album from media lib by name
findAlbum :: String -> MediaLib -> Maybe A.Album
findAlbum atit = retval . filter ((==) atit . A.title) . albums
    where retval [] = Nothing
          retval as = Just (head as)

{---------------
Track operations
-----------------}

-- get the album of a track on first occurence
findTrackAlbum :: String -> MediaLib -> Maybe A.Album
findTrackAlbum ttit = retval . filter ((/=) Nothing . A.findTrack ttit) . albums
    where retval [] = Nothing
          retval as = Just (head as)

-- adds track to an album in a media lib
addTrack :: T.Track -> String -> MediaLib -> MediaLib
addTrack t atit m = addToAlbum . findAlbum atit $ m
    where addToAlbum Nothing = updateAlbum (A.addTrack t (A.Album atit [])) m -- album does not exist
          addToAlbum (Just a)  = updateAlbum (A.addTrack t a) m -- album exists

-- removes a track from a media lib by name
removeTrack :: T.Track -> MediaLib -> MediaLib
removeTrack t m = update . findTrackAlbum (T.title t) $ m
    where update Nothing   = m
          update (Just a)  = updateAlbum (A.removeTrack (T.title t) a) m

-- updates a track in a media lib
updateTrack :: T.Track -> MediaLib -> MediaLib
updateTrack t m = addTrack t (talbum . findTrackAlbum (T.title t) $ m) (removeTrack t m)
    where talbum (Just a) = A.title a
          talbum Nothing = "NoAlbum"

-- adds a rating for a track
rateTrack :: T.User -> T.Track -> T.RatingValue -> MediaLib -> MediaLib
rateTrack u t r = updateTrack (T.rateTrack (T.Rating u r) t)

{---------------
MediaLib operations
-----------------}

-- maps albums and their duration
albumDurations :: MediaLib -> [(String, Int)]
albumDurations = map (\a -> (A.title a, A.duration a)) . albums

-- return all album titles that have at least 50% good tracks
goodAlbums :: T.User -> MediaLib -> [String]
goodAlbums u = map (A.title) . filter ((<=) 0.5 . A.goodRatingRatio u) . albums

-- import single track
importTrack :: (Maybe String, String, String, Int) -> MediaLib -> MediaLib
importTrack (Just atit, ttit, art, dur) = addTrack (T.Track ttit art dur []) atit
importTrack (Nothing, ttit, art, dur)   = importTrack (Just "NoAlbum", ttit, art, dur)

-- import tracks from exercise DB
importTracks :: TL.TrackList -> MediaLib -> MediaLib
importTracks [] m     = m
importTracks (x:xs) m = importTrack x . importTracks xs $ m
