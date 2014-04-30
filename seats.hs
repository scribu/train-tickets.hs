module Seats
( emptyCoach
, purchaseSeats
, showCoach
, Coach
) where

import Text.Printf
import Data.List
import Data.List.Split

data Coach = Coach { seats :: [(Int, Bool)], seatsPerComp :: Int } deriving (Show)

emptyCoach :: Int -> Int -> Coach
emptyCoach numComp seatsPerComp = Coach [(i, False) | i <- [1..numComp*seatsPerComp]] seatsPerComp

compartments :: Coach -> [[(Int, Bool)]]
compartments (Coach seats seatsPerComp) = chunksOf seatsPerComp seats

showSeat :: (Int, Bool) -> [Char]
showSeat (nr, occupied) = printf "%02d %s" nr (if occupied then "Y" else "N")

showCompartment :: [(Int, Bool)] -> [Char]
showCompartment seats = intercalate "\n" $ map showSeat seats

showCoach :: Coach -> [Char]
showCoach coach = intercalate "\n----\n" $ map showCompartment $ compartments coach

emptySeats :: [(Int, Bool)] -> [Int]
emptySeats seats = foldr (\(nr, occupied) acc -> if occupied then acc else nr:acc)
    [] seats

findEmptyCompartment :: Coach -> Int -> Maybe [(Int, Bool)]
findEmptyCompartment coach seatsToBuy = find emptyCompartment $ compartments coach
    where emptyCompartment compartment = seatsToBuy <= length (emptySeats compartment)

findEmptySeats :: Coach -> Int -> [Int]
findEmptySeats coach seatsToBuy =
    case attemptedFill of (Just compartment) -> seatList compartment
                          Nothing            -> seatList (seats coach)
    where attemptedFill = findEmptyCompartment coach seatsToBuy
          seatList seats = take seatsToBuy $ emptySeats seats

occupySeats :: [Int] -> Coach -> Coach
occupySeats seatsToMark (Coach seats seatsPerComp) = Coach (map matchSeat seats) seatsPerComp
    where matchSeat (nr, True) = (nr, True)
          matchSeat (nr, False) = (nr, elem nr seatsToMark)

purchaseSeats :: Coach -> Int -> (Coach, Maybe [Int])
purchaseSeats coach seatsToBuy = if (length foundSeats) < seatsToBuy
    then (coach, Nothing)
    else (occupySeats foundSeats coach, Just foundSeats)
    where foundSeats = findEmptySeats coach seatsToBuy
