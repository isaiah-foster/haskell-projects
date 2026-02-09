import Debug.Trace


inc x = x + 1
dec x = x - 1

id x = inc (dec x)

printInc = print (inc 5)
printDec = print (dec 5)

foo x = double $ inc $ dec x
    where double y = 2*y


xs = [1,2,3,4,5]

dropLs n xs | trace ("\nn = " ++ show n ++ ", xs = " ++ show xs++"\n") False = undefined

dropLs n [] = []
dropLs n ls@(x:xs)
    | n > 0 = dropLs (n-1) xs
    | otherwise = ls



rev [] = []

rev (x:xs) = (rev xs) ++ [x]



revs xs = revHelper xs []
    where
        revHelper [] acc = acc
        revHelper (x:xs) acc = revHelper xs (x:acc)


--Odds and Evens
-- oddEvens [1,2,3,4] = ([1,3], [2,4])

oddEvens xs = (odds,evens)
    where 
        odds = [x | x <- xs, x `mod` 2 ==1]
        evens = [x | x <- xs, x `mod` 2==0]