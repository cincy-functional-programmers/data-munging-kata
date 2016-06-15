module Munge where

import Data.Char (isDigit)	
import Data.List (sortBy)

data Item = Item String Int Int deriving Show

stripNonDigits = filter isDigit

parseLine :: Int -> Int -> Int -> String -> Item
parseLine nameidx aidx bidx line = 
	let elems   = words line
	    daystr  = elems !! nameidx
	    maxtstr = stripNonDigits $ elems !! aidx
	    mintstr = stripNonDigits $ elems !! bidx
	    maxt    = read maxtstr :: Int
	    mint    = read mintstr :: Int in
	    Item daystr maxt mint

parseWeatherLine = parseLine 0 1 2
parseStandingLine = parseLine 1 6 8

itemSpread :: Item -> Int
itemSpread (Item _ a b) = abs (a - b)	    

itemOrdering :: Item -> Item -> Ordering
itemOrdering i1 i2 =
	let i1s = itemSpread i1
	    i2s = itemSpread i2 in
	    compare i1s i2s

smallest = head . (sortBy itemOrdering)	    	    

isValidLine :: String -> Bool
isValidLine [] = False
isValidLine line =
	let elems   = words line
	    first   = elems !! 0 in
	    isDigit $ head first

loadFile :: String -> (String -> Item) -> IO ()
loadFile fileName parseLine = do 
	content <- readFile fileName
	let fileLines = filter isValidLine $ lines content
	let items = fmap parseLine fileLines    
	print $ smallest items
