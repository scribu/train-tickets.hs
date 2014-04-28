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

showSeat (nr, occupied) = printf "%02d %s" nr (if occupied then "Y" else "N")

showCompartment seats = intercalate "\n" $ map showSeat seats

showCoach :: Coach -> [Char]
showCoach (Coach seats seatsPerComp) = intercalate "\n----\n" $ map showCompartment $
    chunksOf seatsPerComp seats

allEmptySeats seats = foldr isEmpty [] seats
    where isEmpty (_, True) acc = acc
          isEmpty (nr, False) acc = nr:acc

findEmptySeats (Coach seats seatsPerComp) seatsToBuy = take seatsToBuy $ allEmptySeats seats

occupySeats seatsToMark (Coach seats seatsPerComp) = Coach (map matchSeat seats) seatsPerComp
    where matchSeat (nr, True) = (nr, True)
          matchSeat (nr, False) = (nr, elem nr seatsToMark)

purchaseSeats :: Coach -> Int -> (Coach, Maybe [Int])
purchaseSeats coach seatsToBuy = if (length foundSeats) < seatsToBuy
    then (coach, Nothing)
    else (occupySeats foundSeats coach, Just foundSeats)
    where foundSeats = findEmptySeats coach seatsToBuy
