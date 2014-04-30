import Test.Hspec
import Seats

main :: IO ()
main = hspec $ do
    describe "Seat purchasing" $ do
        it "can't buy more seats than the coach has" $ do
            let (_, result) = (emptyCoach 3 2) `purchaseSeats` 7
            result `shouldBe` Nothing

        it "can fill more than the first compartment" $ do
            emptyCoach 3 4 `findEmptySeats` 5 `shouldBe` [1..5]

        it "groups seats in a single compartment" $ do
            emptyCoach 3 4 `occupySeats` [1..3] `findEmptySeats` 4 `shouldBe` [5..8]

        it "groups seats in as few compartments as possible" $ do
            emptyCoach 3 4 `occupySeats` [1..3] `occupySeats` [5..7] `findEmptySeats` 5
            `shouldBe` [9..12] ++ [4]
