-- Isaiah Foster 11796849


-----PART 1-----

--1. Take List
takeList :: Int -> [a] -> [a]
takeList n [] = []
takeList n (x:xs)
    | n>0 = x : takeList (n-1) xs
    | otherwise = []


--2. Split by Condition
splitByCondition :: (a -> Bool) -> [a] -> ([a], [a])
splitByCondition cond [] = ([],[])
splitByCondition  cond (x:xs)
    | cond x = (first, x:second)
    | not (cond x) = (x:first, second)
    where (first,second) = splitByCondition cond xs

--3. Zip Lists
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] [] = []
zipLists _ [] = []
zipLists [] _ = []
zipLists (x:xs) (y:ys) = (x,y) : zipLists xs ys

--4. Interleave Lists
interleaveLists :: [a] -> [a] -> [a]
interleaveLists [] [] = []
interleaveLists (x:xs) [] = if null(xs) then [x] else []
interleaveLists [] (y:ys)= if null(ys) then [y] else []
interleaveLists (x:xs) (y:ys) = x : y : interleaveLists xs ys


-----PART 2-----

--1. Merge Ascending
mergeAscending :: Ord a => [a] -> [a] -> [a]
mergeAscending [] [] = []
mergeAscending (x:xs) [] = x:xs
mergeAscending [] (y:ys) = y:ys
mergeAscending (x:xs) (y:ys)
    | x<=y = x : mergeAscending xs (y:ys)
    | otherwise = y : mergeAscending (x:xs) ys

--2. Merge Descending
mergeDescending :: Ord a => [a] -> [a] -> [a]
mergeDescending [] [] = []
mergeDescending (x:xs) [] = x:xs
mergeDescending [] (y:ys) = y:ys
mergeDescending (x:xs) (y:ys)
    | x>=y = x : mergeDescending xs (y:ys)
    | otherwise = y : mergeDescending (x:xs) ys
    
--3. Merge Sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    mergeAscending (mergeSort first) (mergeSort second)
    where 
        touple = splitInHalf xs
        first = fst touple
        second = snd touple

--4. Insertion Sort
--insertionSort :: Ord a => [a] -> [a]


-----TEST FUNCTIONS-----

testAll = do
    testSplitByCondition
    testZipLists
    testInterleaveLists
    testMergeAscending

testSplitByCondition =
    if 
        splitByCondition isEven [-2,-1,0,1,2,3] == ([-1,1,3],[-2,0,2])&&
        splitByCondition isPrime [0,1,2,3,7,8] == ([0,1,8],[2,3,7])
    then print "Split by condition test passed"
    else print "Split by condition test failed"

testZipLists = 
    if 
        zipLists [1,3,5,7,9,11] [2,4,6] == [(1,2),(3,4),(5,6)]&&
        null (zipLists [1,2] [])&&
        null (zipLists[][])&&
        zipLists [1,2] [3,4] == [(1,3),(2,4)]

    then print "Zip lists test passed"
    else print "Zip lists test failed"

testInterleaveLists = 
    if
        interleaveLists [1,3,5] [2,4,6,7,8] == [1,2,3,4,5,6]&&
        null (interleaveLists [] [1,2,3,4])&&
        interleaveLists [1,2,3] [4,5,6,7] == [1,4,2,5,3,6,7]&&
        null(interleaveLists [][])

    then print "Interleave lists test passed"
    else print "Interleave lists test failed"

testMergeAscending = 
    if
        mergeAscending [1,3,5,7,9] [2,4,6,8,10] == [1,2,3,4,5,6,7,8,9,10]&&
        mergeAscending [] [1,2,3,4] == [1,2,3,4]&&
        mergeAscending [-100,1,100] [-1,0,1] == [-100,-1,0,1,1,100]
    then print "Merge ascending test passed"
    else print "Merge Acending test failed"

-----HELPER FUCTIONS-----

isEven :: Integer -> Bool
isEven x = mod x 2 ==0

isPrime :: Int -> Bool
isPrime x
    | elem x [0,1] = False
    | x==2 = True
    | otherwise = primeHelper x 2

primeHelper :: Int -> Int -> Bool
primeHelper x inc
    | fromIntegral inc > sqrt (fromIntegral x) = True -- found fromIntegral online from stackoverflow
    | mod x inc == 0 = False
    | otherwise = primeHelper x (inc+1)


splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt ((length xs+1) `div` 2) xs