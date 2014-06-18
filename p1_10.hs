import Test.QuickCheck

-- 1
myLast :: [a] -> a
myLast [] = error "no last element" 
myLast [x] = x
myLast (x:xs) = myLast xs

testMyLast :: Int -> [Int] -> Bool
testMyLast x xs = (length xs < 1) || (x == (myLast $ xs ++ [x]))

doTestMyLast :: IO ()
doTestMyLast = quickCheck testMyLast

-- 2
myButLast :: [a] -> a
myButLast [] = error "no butlast element" 
myButLast (x:[y]) = x
myButLast (x:xs) = myButLast xs

-- Q: how to restrict the range of xs?
testMyButLast :: Int -> Int -> [Int] -> Bool
testMyButLast x y xs = (length xs < 2) || (x == (myButLast $ xs ++ x:[y]))

doTestMyButLast :: IO ()
doTestMyButLast = quickCheck testMyButLast

-- 3

elementAt :: [a] -> Int -> a
elementAt [] _ = error "index out of range"
elementAt (x:xs) 0 = x
elementAt (x:xs) i = elementAt xs $ i - 1

doTestElementAt :: IO ()
doTestElementAt = quickCheck testElementAt

testElementAt :: [Int] -> Int -> Bool
testElementAt xs i = (i < 0) || (i >= length xs) || xs !! i == (elementAt xs i)

-- 4


