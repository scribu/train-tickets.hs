module Seats
( emptyCoach
, purchaseSeats
, showCoach
) where

import Text.Printf
import Data.List
import Data.List.Split

data Coach = Coach { seats :: [(Int, Bool)], seatsPerComp :: Int } deriving (Show)

emptyCoach :: Int -> Int -> Coach
emptyCoach numComp seatsPerComp = Coach [(i, False) | i <- [1..numComp*seatsPerComp]] seatsPerComp

compartments (Coach seats seatsPerComp) = chunksOf seatsPerComp seats

showSeat (nr, occupied) = printf "%02d %s" nr (if occupied then "Y" else "N")

showCompartment seats = intercalate "\n" $ map showSeat seats

showCoach :: Coach -> [Char]
showCoach coach = intercalate "\n----\n" $ map showCompartment $ compartments coach

allEmptySeats coach = foldr (\(nr, occupied) acc -> if occupied then acc else nr:acc)
    [] (seats coach)

findEmptySeats coach seatsToBuy = take seatsToBuy $ allEmptySeats coach

occupySeats seatsToMark (Coach seats seatsPerComp) = Coach (map matchSeat seats) seatsPerComp
    where matchSeat (nr, True) = (nr, True)
          matchSeat (nr, False) = (nr, elem nr seatsToMark)

purchaseSeats :: Coach -> Int -> (Coach, Maybe [Int])
purchaseSeats coach seatsToBuy = if (length foundSeats) < seatsToBuy
    then (coach, Nothing)
    else (occupySeats foundSeats coach, Just foundSeats)
    where foundSeats = findEmptySeats coach seatsToBuy
