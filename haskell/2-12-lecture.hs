--Data types practice 

type Name = String

addSalutation:: Name->String
addSalutation n = "Dr. " ++ n

--Effectively an alias
type Point = (Double, Double)

type Student = (Int, String, String)

--
--newtype USD = USD Double
--newtype INR = INR Double
--newtype EUR = EUR Double

--convert(INR x) = USD $ x*90
--convert (USD x) = INR $ x/90

--Data allows multiple constructors for one type
data Currency = USD Double 
    | INR Double 
    | EUR Double
    deriving (Show, Eq)

convert :: Currency -> Currency
convert (USD x) = USD x
convert (INR x) = USD $ x/90
convert (EUR x) = USD $ x*0.9

-- instance Num Currency where
--     (+) (USD x) (USD y) = USD (x+y)
-- -- 
data Temperature = Fahrenheit Double
    | Celcius Double
    | Kevlin Double
    deriving (Show, Eq)

data Shape = 
      Circle Double
    | Rectangle Double Double
    | Square Double 
    deriving (Show, Eq)

c = Circle 10
r = Rectangle 10 20
s = Square 10

area (Square s) = s*s
area (Rectangle l h) = l*h
area (Circle r) = pi *r*r

area' shape = case shape of
    Circle r -> pi *r*r
    Square s -> s*s 
    Rectangle l h -> l*h 

totalCost shape lcost =
    let materialCost = case shape of 
            Circle r -> area shape * 5.0
            _ -> (area shape) * 4.0
        laborCost = case lcost of 
            USD amt -> amt
            INR amt -> amt / 90
            EUR amt -> amt*0.9
    in USD $ materialCost + laborCost

-- data Customer = Customer String Int String
--     deriving Show

-- customer = Customer "Alex" 20 "alex@gmail.com"

-- getCustomerName (Customer name _ _) = name


data Customer = Customer{
    customerName :: String,
    customerAge :: Int,
    customerEmail :: String
}deriving (Show,Eq)

--constructor (order doesnt matter)
customer = Customer{
    customerAge = 20,
    customerEmail = "alex@gmail.com",
    customerName = "Alex"
}


customer1 = customer {
    customerEmail = "alex@wsu.edu"
}

data Box a = Box a deriving Show

data Color = Red | Blue | Green

printColor::Color->String
printColor Red = "it's red"
printColor Green = "it's green"
printColor Blue = "it's blue"


data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

btree = Node 2 
        (Node 1 Empty Empty) 
        (Node 3 Empty Empty)

treeSize Empty = 0
treeSize (Node _ left right) = 1+ (treeSize left) + (treeSize right)

treeDepth Empty = 0
treeDepth (Node _ left right) = 1+ max (treeDepth left) (treeDepth right)

treeFind _ Empty = False
treeFind n (Node x left right) = ( n == x) || treeFind n left || treeFind n right

type Peg = Char
type Move = (Int, Peg, Peg)

hanoi 0 _ _ _ = []
hanoi n source dest aux = 
    hanoi (n-1) source aux dest ++
    [(n, source, dest)] ++
    hanoi (n-1) aux dest source