import Test.QuickCheck
import Test.QuickCheck.Gen (choose)

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

myLength :: [a] -> Int
myLength xs = len' xs 0
  where len' [] i = i
        len' (x:xs) i = len' xs $ i + 1

doTestMyLength :: IO ()
doTestMyLength = quickCheck testMyLength

testMyLength :: [Int] -> Bool
testMyLength xs = length xs == myLength xs

-- 5

myReverse :: [a] -> [a]
myReverse xs = rev xs []
  where rev [] acc = acc
        rev (x:xs) acc = rev xs (x:acc)

doTestMyReverse :: IO ()
doTestMyReverse = quickCheck testMyReverse

testMyReverse :: [Int] -> Bool
testMyReverse xs = reverse xs == myReverse xs

-- 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

doTestIsPalindrome :: IO ()
doTestIsPalindrome = do
  quickCheck testIsPalindrome1
  quickCheck testIsPalindrome2

testIsPalindrome1 :: [Int] -> Bool
testIsPalindrome1 xs = isPalindrome (xs ++ reverse xs) == True

testIsPalindrome2 :: [Int] -> Bool
testIsPalindrome2 xs = isPalindrome (1 : xs ++ [2]) == False

-- 7

data NestedList a = Elem a
                  | List [NestedList a]
                  deriving (Eq, Show)

flatten :: NestedList a -> [a]
flatten x = myReverse $ flatten' [x] [] []
  where flatten' :: [NestedList a] -> [[NestedList a]] -> [a] -> [a]
        flatten' ((Elem x):xs) queue acc = flatten' xs queue (x:acc)
        flatten' ((List xxs):xs) queue acc = flatten' xxs (xs:queue) acc
        flatten' [] (xs:queue) acc = flatten' xs queue acc
        flatten' [] [] acc = acc

newtype Boo = Boo (NestedList Int, [Int]) deriving (Eq, Show)

booNested (Boo (n, _)) = n
booList (Boo (_, l)) = l

instance Arbitrary Boo where
  arbitrary = do
    nest <- choose(True, False)
    if nest then
     do
      nested <- (arbitrary :: Gen [Boo] )
      return $ Boo (List $ map booNested nested, foldl (++) [] $ map booList nested)
    else
     do
      val <- (arbitrary :: Gen Int)
      return $ Boo (Elem val, [val])

-- 8
      
elementAt :: [a] -> Int -> a
elementAt [] _ = error "index out of range"
elementAt (x:xs) 0 = x
elementAt (x:xs) i = elementAt xs (i - 1)



