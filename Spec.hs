import Test.Hspec
import Seats

main :: IO ()
main = hspec $ do
    describe "Seat purchasing" $ do
        it "can't buy more seats than the coach has" $ do
            let (_, result) = purchaseSeats (emptyCoach 3 2) 7
            result `shouldBe` Nothing

        it "can fill more than the first compartment" $ do
            findEmptySeats (emptyCoach 3 4) 5 `shouldBe` [1..5]

        it "groups seats in a single compartment" $ do
            findEmptySeats (occupySeats [1..3] $ emptyCoach 3 4) 4 `shouldBe` [5..8]
