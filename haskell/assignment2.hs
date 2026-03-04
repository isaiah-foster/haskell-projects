-- Assignment: CPTS_S 355 Assignment 2 
-- Student: Isaiah Foster 
-- Student ID: 11796849 

--define mass datatype 
data Mass = KG Double | LB Double 
    deriving (Show, Eq)


--define custom Ord for Mass  datatype
instance Ord Mass where
    (KG kg1) `compare` (KG kg2) = compare kg1 kg2
    (LB lb1) `compare` (LB lb2) = compare lb1 lb2
    (KG kg) `compare` (LB lb) = compare kg (lb / 2.20462)
    (LB lb) `compare` (KG kg) = compare (lb / 2.20462) kg

instance Num Mass where
    (+) :: Mass -> Mass -> Mass
    (KG x) + (KG y) = KG (x+y)
    (LB x) + (LB y) = LB (x+y)
    (KG x) + (LB y) = KG (x+ y/2.20462)
    (LB x) + (KG y) = KG (x/2.20462 +y)


--conversions between pounds and kgs
convertToLB :: Mass -> Mass
convertToLB (KG kg) = LB (kg * 2.20462)
convertToLB x = x

convertToKG :: Mass -> Mass
convertToKG (LB lb) = KG (lb / 2.20462)
convertToKG x = x
    

--define bst datatype
data BST = Empty | Node Mass BST BST deriving (Show, Eq)


--insert function for a node into the tree, using my Ord for comparison
insert :: Mass -> BST -> BST
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

--search function to check if a mass exists in a tree
search :: Mass -> BST -> Bool
search x Empty = False
search x (Node y left right)
    | x == y = True
    | x<y = search x left
    | otherwise = search x right

--sum all values in the tree to a KG value
sumTree:: BST -> Mass
sumTree Empty = KG 0
sumTree (Node x left right) = (convertToKG x) + (sumTree left) + (sumTree right)
