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
type Compartment = [Seat]
data Coach = Coach { seats :: [Seat], seatsPerComp :: Int } deriving (Show)

emptyCoach :: Int -> Int -> Coach
emptyCoach numComp seatsPerComp = Coach [(i, False) :: Seat | i <- [1..numComp*seatsPerComp]] seatsPerComp

compartments :: Coach -> [Compartment]
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

splitNr :: Int -> Int -> [Int]
splitNr nr partition = take (nr `div` partition) (repeat partition) ++
    (if remainder > 0 then [remainder] else [])
    where remainder = nr `mod` partition

goForward :: [Compartment] -> [Int] -> [SeatNr] -> ([Int], [SeatNr])
goForward comp [] seats = ([], seats)
goForward [] toBuy seats = (toBuy, seats)
goForward (comp:ct) (toBuy:leftToBuy) markedSeats =
    let foundSeats = emptySeats comp in
    if (toBuy <= length foundSeats) then
        goForward ct leftToBuy (markedSeats ++ (take toBuy foundSeats))
    else
        goForward ct (toBuy:leftToBuy) markedSeats

findEmptySeats :: Coach -> Int -> [SeatNr]
findEmptySeats coach seatsToBuy =
    let seatChunks = (splitNr seatsToBuy (seatsPerComp coach))
        (notBought, foundSeats) = goForward (compartments coach) seatChunks []
    in foundSeats ++ take (sum notBought) ((emptySeats $ seats coach) \\ foundSeats)

occupySeats :: Coach -> [SeatNr] -> Coach
occupySeats (Coach seats seatsPerComp) seatsToMark = Coach (map matchSeat seats) seatsPerComp
    where matchSeat (nr, True) = (nr, True)
          matchSeat (nr, False) = (nr, elem nr seatsToMark)

purchaseSeats :: Coach -> Int -> (Coach, Maybe [SeatNr])
purchaseSeats coach seatsToBuy = if (length foundSeats) < seatsToBuy
    then (coach, Nothing)
    else (coach `occupySeats` foundSeats, Just foundSeats)
    where foundSeats = findEmptySeats coach seatsToBuy
