import Test.QuickCheck
import Test.QuickCheck.Gen (choose)
import Debug.Trace (trace)
import System.Random (randomIO)
import Data.List (delete)

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

testFlatten :: [(Bool, Int, Int)] -> Bool
testFlatten spec = (flatten $ List $ mkNested spec) == (mkFlat spec)
  where mkFlat spec = map (\(_, _, x) -> x) spec
        mkNested :: [(Bool, Int, Int)] -> [NestedList Int]
        mkNested [] = []
        mkNested ((False, _, elt):xs) = (Elem elt) : (mkNested xs)
        mkNested ((True, len, elt):xs) =
          let (ys, zs) = splitAt (((abs len) + 1) `mod` 20) xs
           in
              (Elem elt) : (List $ mkNested ys) : (mkNested zs)

doTestFlatten = quickCheck testFlatten

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

testCompress :: String -> Bool
testCompress [] = True
testCompress l@(x:xs) = test x ((tail . compress) l)
  where test _ [] = True
        test x (y:ys) = (x /= y) && (test y ys)

doTestCompress = quickCheck testCompress

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
              
testPack :: String -> Bool
testPack xs = (foldl (++) [] (pack xs)) == xs

doTestPack = quickCheck testPack

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
              
decode :: [(Int, a)] -> [a]
decode [] = []
decode ((cnt, elt):xs) = (repeatN cnt elt) ++ (decode xs)

testEncode :: String -> Bool
testEncode xs = ((decode . encode) xs) == xs

doTestEncode = quickCheck testEncode

-- 11

data Elt a = Multiple Int a
           | Single a
           deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Elt a) where
  arbitrary = do
    nest <- choose(True, False)
    elt <- (arbitrary :: Arbitrary a => Gen a)
    if nest then
      do
        cnt <- (arbitrary :: Gen Int)
        return $ Multiple ((abs cnt) `mod` 10 + 2) elt
    else
      return $ Single elt

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

testEncodeDecodeModified :: [Elt Int] -> Bool
testEncodeDecodeModified [] = True
testEncodeDecodeModified (x:xs) = 
  let ys = removeDuplicates x xs in
    (encodeModified . decodeModified) ys == ys
  where removeDuplicates _ [] = []
        removeDuplicates prev_elt (x:xs) =
          if prev_elt == x then
            removeDuplicates x xs
          else
            prev_elt : (removeDuplicates x xs)

doTestEncodeDecodeModified = quickCheck testEncodeDecodeModified

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

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt elm list 0 = elm:list
insertAt elm [] _ = [elm]
insertAt elm (x:xs) idx = x:(insertAt elm xs $ idx - 1)

-- 22
range :: Int -> Int -> [Int]
range x y | x == y = [x]
          | x < y = x:(range (x+1) y)
          | x > y = []

-- 23 v1
rnd_select :: [a] -> Int -> IO ([a])
rnd_select list num = select list $ mkPositions (num `min` max_index) []
    where max_index = length list -- Q: how to get rid of this? can'r be used on infinite lists
          select :: [a] -> IO ([Int]) -> IO ([a])
          -- Q: why I can't remove 'list' parameter from select func
          select list positions' = do
              positions'' <- positions'
              case positions'' of
                  [] -> return []
                  (p:positions) -> do
                      tail <- (select list $ return positions)
                      return $ (list !! p) : tail
          mkPositions :: Int -> [Int] -> IO ([Int])
          mkPositions 0 used = return []
          mkPositions n used = do
              val' <- randomIO :: IO (Int)
              let val = val' `mod` max_index in
                  if elem val used then
                    mkPositions n used
                  else
                    do
                        tail <- mkPositions (n-1) (val:used)
                        return $ val:tail
-- 24
diff_select :: Int -> Int -> IO ([Int])
diff_select num maximum = rnd_select (range 1 maximum) num

-- 25
rnd_permu :: [a] -> IO([a])
rnd_permu list = do
    let size = length list
    indexes <- rnd_select (range 0 (size-1)) size
    return $ map (\idx -> list !! idx) indexes

-- 26
combinations :: Eq a => Int -> [a] -> [[a]]
combinations 1 list = map (\x -> [x]) list
combinations _ [] = []
combinations num list =
  [x:sublist | x <- list, sublist <- (combinations (num - 1) (delete x list))]

   
-- -- | The main entry point.
-- main :: IO ()
-- main = do
-- --    putStrLn $ show $ insertAt 1 [] 10
-- --    putStrLn $ show $ insertAt 1 [10,20] 0
-- --    putStrLn $ show $ insertAt 1 [10,20] 1
-- --    putStrLn $ show $ insertAt 1 [10,20] 2
-- --    putStrLn $ show $ insertAt 1 [10,20] 3
-- --    putStrLn $ show $ range 9 0
-- --    putStrLn $ show $ range 1 1
-- --    putStrLn $ show $ range 1 5
--     rnd_select (range 0 9) 3 >>= putStrLn.show
--     rnd_select (range 0 9) 0 >>= putStrLn.show
--     rnd_select (range 0 9) 30 >>= putStrLn.show
-- --    diff_select 5 100 >>= putStrLn.show
-- --    rnd_permu "abcdef" >>= putStrLn.show
-- --    rnd_permu "abcdef" >>= putStrLn.show
