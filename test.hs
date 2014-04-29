import Seats
import Test.HUnit

testTooMany = TestCase $ assertEqual "Shouldn't buy more seats than the coach has" Nothing result
    where (_, result) = purchaseSeats (emptyCoach 3 2) 7

main = runTestTT testTooMany
