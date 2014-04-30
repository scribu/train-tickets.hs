import System.IO
import Data.List
import Seats

loop coach = do
    putStrLn $ showCoach coach
    putStr "Number of seats to buy: "
    hFlush stdout
    input <- getLine
    let (newCoach, result) = purchaseSeats coach $ read input
    case result of (Just seatList) -> putStrLn $ boughtSeats seatList
                   _               -> putStrLn "Not enough seats available"
    loop newCoach
    where boughtSeats seatList = "Bought seats: " ++ (intercalate " " $ map show seatList)

main = loop $ emptyCoach 5 5
