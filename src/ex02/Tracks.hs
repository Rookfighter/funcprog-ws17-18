-- Tracks.hs
--
--     Author: Fabian Meyer
-- Created on: 26 Oct 2017

module Tracks where

type TrackList = [(Maybe String, String, String, Int)]
-- no album:
noAlbum :: TrackList
noAlbum =
  [ (Nothing, "Cosmic Cars (Detroit Style)","Cybotron",267)
  , (Nothing, "Dream Girl","VARIOUS",528)
  , (Nothing, "Boddika's House","BODDIKA",352)
  , (Nothing, "The Alps","Braiden",411)
  , (Nothing, "Groove Of The Ghetto","A GUY CALLED GERALD",373)
  , (Nothing, "A1 - The Final Frontier","UR",497)
  ]


-- album: Electronic Warfare 2.0
electronic :: TrackList
electronic =
  [ (Just "Electronic Warfare 2.0", "Kut","UNDERGROUND RESISTANCE",299)
  , (Just "Electronic Warfare 2.0", "Technology Gap","UNDERGROUND RESISTANCE",275)
  , (Just "Electronic Warfare 2.0", "Kill My Radio Station","UNDERGROUND RESISTANCE",265)
  ]


-- album: Shifted Phases
shifted :: TrackList
shifted =
  [ (Just "Shifted Phases", "Implosive Regions","Drexciya",298)
  , (Just "Shifted Phases", "Scattering Pulsars","Drexciya",308)
  ]


-- album: The Collective
collective :: TrackList
collective =
  [ (Just "The Collective", "Point Blank","OCTAVE ONE",445)
  , (Just "The Collective", "Meridian","OCTAVE ONE",286)
  , (Just "The Collective", "Nicolette","OCTAVE ONE",273)
  ]

tracklist :: TrackList
tracklist =
  noAlbum ++
  electronic ++
  shifted ++
  collective
