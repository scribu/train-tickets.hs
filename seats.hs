module Seats
( emptyCoach
, findEmptySeats
, occupySeats
, purchaseSeats
, showCoach
, Coach
) where

import Text.Printf
import Data.List
import Data.List.Split

type SeatNr = Int
type Seat = (SeatNr, Bool)
data Coach = Coach { seats :: [Seat], seatsPerComp :: Int } deriving (Show)

emptyCoach :: Int -> Int -> Coach
emptyCoach numComp seatsPerComp = Coach [(i, False) :: Seat | i <- [1..numComp*seatsPerComp]] seatsPerComp

compartments :: Coach -> [[Seat]]
compartments (Coach seats seatsPerComp) = chunksOf seatsPerComp seats

showSeat :: Seat -> [Char]
showSeat (nr, occupied) = printf "%02d %s" nr (if occupied then "Y" else "N")

showCompartment :: [Seat] -> [Char]
showCompartment seats = intercalate "\n" $ map showSeat seats

showCoach :: Coach -> [Char]
showCoach coach = intercalate "\n----\n" $ map showCompartment $ compartments coach

emptySeats :: [Seat] -> [SeatNr]
emptySeats seats = foldr (\(nr, occupied) acc -> if occupied then acc else nr:acc)
    [] seats

findEmptyCompartment :: Coach -> Int -> Maybe [Seat]
findEmptyCompartment coach seatsToBuy = find emptyCompartment $ compartments coach
    where emptyCompartment compartment = seatsToBuy <= length (emptySeats compartment)

findEmptySeats :: Coach -> Int -> [SeatNr]
findEmptySeats coach seatsToBuy =
    case attemptedFill of (Just compartment) -> seatList compartment
                          Nothing            -> seatList (seats coach)
    where attemptedFill = findEmptyCompartment coach seatsToBuy
          seatList seats = take seatsToBuy $ emptySeats seats

occupySeats :: Coach -> [SeatNr] -> Coach
occupySeats (Coach seats seatsPerComp) seatsToMark = Coach (map matchSeat seats) seatsPerComp
    where matchSeat (nr, True) = (nr, True)
          matchSeat (nr, False) = (nr, elem nr seatsToMark)

purchaseSeats :: Coach -> Int -> (Coach, Maybe [SeatNr])
purchaseSeats coach seatsToBuy = if (length foundSeats) < seatsToBuy
    then (coach, Nothing)
    else (coach `occupySeats` foundSeats, Just foundSeats)
    where foundSeats = findEmptySeats coach seatsToBuy
