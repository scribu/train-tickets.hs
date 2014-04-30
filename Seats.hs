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
data Coach = Coach { seats :: [Seat], compSize :: Int } deriving (Show)

emptyCoach :: Int -> Int -> Coach
emptyCoach numComp compSize = Coach [(i, False) | i <- [1..numComp*compSize]] compSize

compartments :: Coach -> [Compartment]
compartments (Coach seats compSize) = chunksOf compSize seats

showSeat :: Seat -> [Char]
showSeat (nr, occupied) = printf "%02d %s" nr (if occupied then "Y" else "N")

showCompartment :: [Seat] -> [Char]
showCompartment seats = intercalate "\n" $ map showSeat seats

showCoach :: Coach -> [Char]
showCoach coach = intercalate "\n----\n" $ map showCompartment $ compartments coach

emptySeats :: [Seat] -> [SeatNr]
emptySeats seats = foldr fn [] seats
    where fn (_, True) acc = acc
          fn (nr, False) acc = nr:acc

splitNr :: Int -> Int -> [Int]
splitNr nr partition = take (nr `div` partition) (repeat partition) ++
    (if remainder > 0 then [remainder] else [])
    where remainder = nr `mod` partition

goForward :: [Compartment] -> [Int] -> [SeatNr] -> ([Int], [SeatNr])
goForward _ [] seats = ([], seats)
goForward [] toBuy seats = (toBuy, seats)
goForward (comp:ct) (toBuy:leftToBuy) markedSeats =
    let foundSeats = emptySeats comp in
    if toBuy <= length foundSeats then
        goForward ct leftToBuy $ markedSeats ++ (take toBuy foundSeats)
    else
        goForward ct (toBuy:leftToBuy) markedSeats

findEmptySeats :: Coach -> Int -> [SeatNr]
findEmptySeats coach seatsToBuy =
    let seatChunks = (splitNr seatsToBuy (compSize coach))
        (notBought, foundSeats) = goForward (compartments coach) seatChunks []
    in foundSeats ++ take (sum notBought) ((emptySeats $ seats coach) \\ foundSeats)

occupySeats :: Coach -> [SeatNr] -> Coach
occupySeats (Coach seats compSize) seatsToMark = Coach (map matchSeat seats) compSize
    where matchSeat (nr, True) = (nr, True)
          matchSeat (nr, False) = (nr, elem nr seatsToMark)

purchaseSeats :: Coach -> Int -> (Coach, Maybe [SeatNr])
purchaseSeats coach seatsToBuy =
    let foundSeats = findEmptySeats coach seatsToBuy in
    if length foundSeats < seatsToBuy then
        (coach, Nothing)
    else
        (coach `occupySeats` foundSeats, Just foundSeats)
