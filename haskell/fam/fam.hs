import Data.Char (toUpper)
data Box a = Empty | Box a
    deriving Show

intBox :: Box Integer
intBox = Box 2

stringBox:: Box String
stringBox = Box "subu"

listIntBox:: Box [Int]
listIntBox = Box [1,2,3]

intRecip :: Int -> Int
intRecip x = 1 `div` x

safeIntRecip :: Int -> Maybe Int
safeIntRecip 0 = Nothing
safeIntRecip x = Just (1 `div` x)

--data Eitherr a = Left String | Right a

safeIntRecip' :: Int-> Either String Int
safeIntRecip' 0 = Left "Can't divide by 0"
safeIntRecip' x = Right (1 `div` x)

myMaybeInt :: Maybe Int
myMaybeInt = Just 2

double :: Int -> Int
double x = x + x

triple :: Int -> Int
triple x = x + x + x

mDouble:: Maybe Int -> Maybe Int
mDouble Nothing = Nothing
mDouble (Just x) = Just (double x)

mTriple:: Maybe Int -> Maybe Int
mTriple Nothing = Nothing
mTriple (Just x) = Just (triple x)

myFmap:: Maybe a -> (a -> b) -> Maybe b
myFmap Nothing _ = Nothing
myFmap (Just x) fn = Just (fn x)

mString = Just "subu"

safeUpperCase :: String -> Maybe String
safeUpperCase x = Just (map toUpper x)