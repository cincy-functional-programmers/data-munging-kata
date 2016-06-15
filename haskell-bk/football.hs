module Football where

import Data.Char (isDigit)	
import Data.List (sortBy)

data TeamStanding = TeamStanding { teamname :: String, goalsScored :: Int, goalsAllowed :: Int} deriving Show

parseLine :: String -> TeamStanding
parseLine line = 
	let elems   = words line
	    namestr = elems !! 1
	    gsstr   = elems !! 6
	    gastr   = elems !! 8
	    gs      = read gsstr :: Int
	    ga      = read gastr :: Int in
	    TeamStanding namestr gs ga

isValidLine :: String -> Bool
isValidLine line =
	let elems   = words line
	    first   = elems !! 0 in
	    isDigit $ head first

goalSpread :: TeamStanding -> Int
goalSpread ts = abs $ (goalsScored ts) - (goalsAllowed ts)

standingOrdering :: TeamStanding -> TeamStanding -> Ordering
standingOrdering ts1 ts2 =
	let ts1s = goalSpread ts1
	    ts2s = goalSpread ts2 in
	    compare ts1s ts2s

smallest = head . (sortBy standingOrdering)

loadFile = do 
	content <- readFile "football.dat"
	let fileLines = filter isValidLine $ lines content
	let standings = fmap parseLine fileLines
	print $ smallest standings