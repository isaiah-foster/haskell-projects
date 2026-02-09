--simplest problem
import Data.Char()
import Data.Char (toUpper)
charCat 'a' = "first alphabet"
charCat 'z' = "last alphabet"
charCat _   = "middle alphabet"

--case expression
charCat2 c = case c of
    'a' -> "first"
    'z' -> "last"
    _   -> "middle"

--Guards
charCat3 c
    | c == 'a' = "first"
    | c == 'z' = "last"
    | otherwise = "middle"

charCat4 c =
    if c == 'a'
        then "first"
    else if c == 'z'
        then "last"
    else "middle"

x = 20

y = True

z='d'

name = "subu"

--literal
inc = (\x->x+1)

inc5 = (\x->x+5)
inc10 = (\x->x+10)

incMaker = (\incVal-> (\x-> x +incVal))


--F(x,y) = x+2y
--y=2
-- P(x) = x+4

add :: Int -> Int -> Int
add x y = x+y

add' = (\x->(\y->x+y))

--Currying
--Haskell Curry

---F(x) = x+27

---g(x) = F(x)

sum1 = add
num=10
num1=num

-- f(x) = 2(x+1)
-- g(x) = 2x+1
-- k(x) = f(g(f(x))))
-- k = f . g . f

f x = 2*(x+1)
g x = (2*x)+1

k = f (g (f x))

k' = f.g.f

-- point free style
toUpperStr = map toUpper

-- 
doubleList xs = map (*2) xs

-- point free style
doubleList' = map (*2)

--recursive list length finder
mylen [] = 0
mylen (_:xs) = 1 + mylen xs

posNeg [] = ([],[])
posNeg (x:xs)
    | x > 0 = (x:first, second)
    | otherwise = (first, x:second)
    where 
        (first, second) = posNeg xs