import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

pending :: Assertion
pending = assertFailure "(test is pending)"

tests :: TestTree
tests = testGroup "input processor"
    [ testCase "nobody is ever overspent" pending
    , testCase "during payout, projects are independent" pending
    , testCase "doesn't make a pledge if funds are insufficient" pending
    ]
