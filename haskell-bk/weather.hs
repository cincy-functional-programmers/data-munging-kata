module Weather where

import Data.List (sortBy)

data Day = Day { day :: Int, maxt :: Int, mint :: Int } deriving Show

parseLine :: String -> Day
parseLine line = 
	let raw   = words line
	    elems = fmap (filter ((/=) '*')) raw
	    daystr  = elems !! 0
	    maxtstr = elems !! 1
	    mintstr = elems !! 2
	    day     = read daystr :: Int
	    maxt    = read maxtstr :: Int
	    mint    = read mintstr :: Int in
	    Day day maxt mint

tempSpread :: Day -> Int
tempSpread d = (maxt d) - (mint d)

dayOrd :: Day -> Day -> Ordering
dayOrd d1 d2 = 
	let d1s = tempSpread d1
	    d2s = tempSpread d2 in
	    compare d1s d2s

minSpread :: [Day] -> Day
minSpread = head . (sortBy dayOrd)

loadFile = do 
	content <- readFile "weather.dat"
	let fileLines = (drop 2) (lines content)
	let days = fmap parseLine (take 30 fileLines)
	print (minSpread days)