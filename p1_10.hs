import Test.QuickCheck
import Test.QuickCheck.Gen (choose)
import Debug.Trace (trace)

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
      
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = compress' x xs
  where compress' :: Eq a => a -> [a] -> [a]
        compress' x [] = [x]
        compress' x (y:ys) = if x == y then
                                 compress' y ys
                               else
                                 x : (compress' y ys)


-- 9

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack' x [] xs
  where pack' :: Eq a => a -> [a] -> [a] -> [[a]]
        pack' x acc [] = [x:acc]
        pack' x acc (y:ys) =
            if x == y then
              pack' y (y:acc) ys
            else
              (x:acc) : (pack' y [] ys)
              
-- 10

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = encode' x 1 xs
  where encode' :: Eq a => a -> Int -> [a] -> [(Int, a)]
        encode' x cnt [] = [(cnt,x)]
        encode' x cnt (y:ys) =
            if x == y then
              encode' y (cnt+1) ys
            else
              (cnt,x) : (encode' y 1 ys)
              
-- 11

data Elt a = Multiple Int a
           | Single a
           deriving (Eq, Show)

encodeModified :: Eq a => [a] -> [Elt a]
encodeModified [] = []
encodeModified (x:xs) = encodeModified' x 1 xs
  where encodeModified' :: Eq a => a -> Int -> [a] -> [Elt a]
        encodeModified' x cnt [] = [(mkElt cnt x)]
        encodeModified' x cnt (y:ys) =
          if x == y then
            encodeModified' y (cnt+1) ys
          else
            (mkElt cnt x) : (encodeModified' y 1 ys)
        mkElt cnt x =
          if cnt == 1 then
            (Single x)
          else
            (Multiple cnt x)
-- 12

repeatN :: Int -> a -> [a]
repeatN 0 _ = []
repeatN i x = x:(repeatN (i-1) x)

decodeModified :: [(Elt a)] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple cnt x):xs) = (repeatN cnt x) ++ (decodeModified xs)

-- 13

-- ???

-- 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

doTestDupli = quickCheck (testRepli 2)

-- 15

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) cnt = (repeatN cnt x) ++ (repli xs cnt)

testRepli :: Int -> String -> Bool
testRepli n1 xs =
  let n = (abs n1) `mod` 10 + 1
      ys = repli xs n
   in
      all (\i -> (all (\y -> y == xs !! i)
                      (slice ys (n * i) (n * i + n - 1))))
          [0 .. length xs - 1]

doTestRepli = quickCheck testRepli

-- 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n
  where dropEvery' [] _ = []
        dropEvery' (x:xs) 0 = dropEvery' xs n
        dropEvery' (x:xs) i = x:(dropEvery' xs (i-1))

testDropEvery :: Int -> Int -> Bool
testDropEvery l1 n1 = 
  let l = (abs l1)
      n = (abs n1)
      len = (mod (max l n) 100) + 1
      step = (mod (min l n) 10) + 1
      xs = dropEvery [0..len] step
    in
      all (\i -> (xs !! i) == (i + i `div` step)) [0..len - (len `div` step)]

doTestDropEvery = quickCheck testDropEvery

-- 17

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) l = let (head, tail) = split xs (l-1) in
  (x:head, tail)

testSplit :: String -> String -> Bool
testSplit xs ys = (split (xs ++ ys) (length xs)) == (xs, ys)

doTestSplit = quickCheck testSplit

-- 18

slice :: [a] -> Int -> Int -> [a]
slice xs 0 last = fst $ split xs (last + 1)
slice [] _ _ = []
slice (x:xs) first last = slice xs (first - 1) (last - 1) 

testSlice :: String -> String -> String -> Bool
testSlice xs ys zs = (slice (xs ++ ys ++ zs) (length xs) (length xs + length ys - 1)) == ys

doTestSlice = quickCheck testSlice

-- 19

rotate :: [a] -> Int -> [a]
rotate xs n = let (head, tail) = split xs (if n > 0 then n else ((length xs) + n)) in
  tail ++ head

testRotate1 :: String -> String -> Bool
testRotate1 xs ys = (rotate (xs ++ ys) (length xs)) == ys ++ xs

testRotate2 :: String -> String -> Bool
testRotate2 xs ys = (rotate (xs ++ ys) (- (length ys))) == ys ++ xs

doTestRotate = do
  quickCheck testRotate1
  quickCheck testRotate2

-- 20

removeAt :: [a] -> Int -> [a]
removeAt xs idx = let (head, ttail) = split xs idx in
  head ++ (tail ttail)

testRemoveAt :: Char -> String -> String -> Bool
testRemoveAt char head tail = (removeAt (head ++ [char] ++ tail) (length head)) == head ++ tail

doTestRemoveAt = quickCheck testRemoveAt
