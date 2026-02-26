module Main (main) where
import Test.HUnit
import MyLib

---Area where you can change things
addTest::Test
addTest = TestCase (assertEqual "testing adding 1 and 2" 3 (add 1 2))

subTest = TestCase(assertEqual "testing sub of 4 and 2" 2(sub 4 2))

tests = TestList[addTest, subTest]
---End of area where you can change things

main :: IO ()
main = do 
    _ <- runTestTT tests
    return ()
