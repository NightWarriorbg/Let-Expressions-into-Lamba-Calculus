-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]] 
splitSort [] = []
splitSort (x:xs)
 | x < head xs = firstTuple (x:xs): splitSort (drop (length (firstTuple (x:xs)))(x:xs))
 | x > head xs = secondTuple (x:xs): splitSort (drop (length (secondTuple (x:xs)))(x:xs))
 | x == head xs = thirdTuple (x:xs): splitSort (drop (length (thirdTuple (x:xs)))(x:xs))
 | otherwise = []:splitSort []
 where
  firstTuple :: Ord a => [a] -> [a]
  firstTuple [] = []
  firstTuple (x:xs) = x:filter ( > x) (firstTuple xs)
  
  secondTuple :: Ord a => [a] -> [a]
  secondTuple [] = []
  secondTuple (x:xs) = x:filter (< x) (secondTuple xs)
  
  thirdTuple :: Ord a => [a] -> [a]
  thirdTuple [] = []
  thirdTuple (x:xs) = x:filter (== x) (thirdTuple xs)

-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList (x:xs) = subSet x (head xs) 
 where
  subSet :: Eq a => [a] -> [a] -> [a]
  subSet []_ = []
  subSet (x:xs) ys
   | x `elem` ys = x : subSet xs ys
   | otherwise = subSet xs ys

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress [] = False
canProgress (x:xs)
 | sumCredits (x:xs) 0.0 >= 60 = True
 | sumCredits (x:xs) 0.0 < 60 = False
 where
 sumCredits :: [ModuleResult] -> Float -> Float
 sumCredits [] 0.0 = 0
 sumCredits [] _ = 0
 sumCredits (x:xs) n = if mark x < 25
                       then 0 
                       else if mark x < 40 && mark x > 25 
                       then if (credit x + n) <= 15 
                            then sumCredits xs (n + (credit x)) 
                            else 0 
                       else if mark x >= 40 
                       then (credit x) + (sumCredits xs n)
                       else 0
-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify [[]] = Third 
classify (x:xs) 
 | (length (x:xs) == 4) = determineDegree (calculateDegree4 (x:xs))
 | (length (x:xs) == 3) = determineDegree (calculateDegree3 (x:xs))
 where
 calculateDegree4 :: [[ModuleResult]] -> Float
 calculateDegree4 [[]] = 0
 calculateDegree4 (x:xs) = (fromIntegral((sumMarks (head xs)) + 2*(sumMarks (head (tail xs))) + 2*(sumMarks (head (tail (tail (xs)))))))/4

 sumMarks :: [ModuleResult] -> Int
 sumMarks [] = 0
 sumMarks (x:xs)
  | mark x < 40 = sumMarks xs
  | mark x >= 40 = fromIntegral(mark x) + fromIntegral(sumMarks xs)
  
 calculateDegree3 :: [[ModuleResult]] -> Float
 calculateDegree3 [[]] = 0
 calculateDegree3 (x:xs) = (fromIntegral((sumMarks (head xs) + 2*(sumMarks (head (tail xs))))))/3

 determineDegree :: Float -> DegreeClass
 determineDegree x = if x < 50 then Third else if x < 60 && x >= 50 then LowerSecond else if x < 70 && x >= 60 then UpperSecond else if x > 70 then First else Third

-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
{- Solution is inspired by Wikipedia page on golden ratio and golden section search -}
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps
 | abs(a - b) > eps = if (d a) > (d b) then hillClimb d x b eps else hillClimb d a x' eps
 | otherwise = (x + x')/2
 where
  h = if x' > x then x' - x else x - x' :: Float
  a = x' - h / goldenRation :: Float
  b = x + h / goldenRation :: Float
  goldenRation = (1 + sqrt 5) / 2 :: Float

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = 0.0

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns ins
 | ins == [] = ns
executeInstructionSequence (x:xs) (i:is)
 | i == Add = if (length (x:xs) > 1) then executeInstructionSequence (add' (x:xs)) is else xs
 | i == Multiply = if (length (x:xs) > 1) then  executeInstructionSequence (multiply (x:xs)) is else xs
 | i == Duplicate = executeInstructionSequence (duplicate (x:xs)) is
 | i == Subtract = if (length (x:xs) > 1) then executeInstructionSequence (subtract' (x:xs)) is else xs
 | i == Pop = if (length (x:xs) > 1) then executeInstructionSequence (pop (x:xs)) is else []
 | otherwise = (x:xs)
 where
  add' :: [Int] -> [Int]
  add' [] = []
  add'(x:xs) = sum(take 2 (x:xs)):(tail xs)
  
  multiply :: [Int] -> [Int]
  multiply [] = []
  multiply (x:xs) = (x * (head xs)):(tail xs)
  
  duplicate :: [Int] -> [Int]
  duplicate [] = []
  duplicate (x:xs) = x:(x:xs) 
  
  subtract' :: [Int] -> [Int]
  subtract' [] = []
  subtract' (x:xs) = (x - (head xs)):(tail xs)

  pop :: [Int] -> [Int]
  pop [] = []
  pop (x:xs) = tail (x:xs)

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []
optimalSequence n 
 | isEven n == True = generateInstructionEven n
 | isEven n == False = generateInstructionOdd n
 where
  isEven :: Int -> Bool 
  isEven a = (a `mod` 2) == 0
  
  generateInstructionEven :: Int -> [Instruction]
  generateInstructionEven 0 = []
  generateInstructionEven n 
   | n > 1 = Duplicate:Multiply:(generateInstructionEven (n-2))
   | otherwise = []
   
  generateInstructionOdd :: Int -> [Instruction]
  generateInstructionOdd 0 = []
  generateInstructionOdd n 
   | n > 1 = (duplicate n)++(multiply n)
   | otherwise = []
   where
    duplicate :: Int -> [Instruction]
    duplicate 0 = []
    duplicate n
     |n > 1 = Duplicate:(duplicate (n-1))
     |otherwise = []

    multiply :: Int -> [Instruction]
    multiply 0 = []
    multiply n 
     | n > 1 = Multiply:(multiply (n-1))
     | otherwise = []

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [] = []
findBusyBeavers (x:xs)
 | x <= 0 && (length (x:xs)) == 2 = [Pop]:[Add]:findBusyBeavers xs
 | otherwise = findHighestValue (x:xs)
 where
  findHighestValue :: [Int] -> [[Instruction]]
  findHighestValue [] = []
  findHighestValue (x:xs)
   | (length (x:xs)) > 1 && x == 0 = [Pop]:findHighestValue xs
   | (length (x:xs)) > 1 && head (add' (x:xs)) < head (multiply (x:xs)) = [Multiply]:findHighestValue (multiply (x:xs))
   | (length (x:xs)) > 1 && head (add' (x:xs)) > head (multiply (x:xs)) = [Add]:findHighestValue (add' (x:xs))
   | (length (x:xs)) > 1 && head (add' (x:xs)) == head (multiply (x:xs)) = [Add]:[Multiply]:findHighestValue (add' (x:xs))
   | otherwise = []
   where 
    pop :: [Int] -> [Int]
    pop [] = []
    pop (x:xs) = tail (x:xs)

    add' :: [Int] -> [Int]
    add' [] = []
    add'(x:xs) = sum(take 2 (x:xs)):(tail xs)
  
    multiply :: [Int] -> [Int]
    multiply [] = []
    multiply (x:xs) = (x * (head xs)):(tail xs) 

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = []

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []

-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage "" = ""
extractMessage s = revealMessage(decodeMessage s)
 where
  decodeMessage :: String -> String
  decodeMessage "" = ""
  decodeMessage (x:xs)
   | x == '0' = '0':decodeMessage xs
   | x == '1' = '1':decodeMessage xs
   | otherwise = decodeMessage xs

  revealMessage :: String -> String
  revealMessage [] = ""
  revealMessage (x:y:xs)
   | x == '0' && y == '0' = 'a':revealMessage xs
   | x == '0' && y == '1' = 'b':revealMessage xs
   | x == '1' && y == '0' = 'c':revealMessage xs
   | x == '1' && y == '1' = 'd':revealMessage xs

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
differentStream [[]] = []
differentStream ss = [(ss !! n) !! n + 1 | n <- [0,1..]] -- definies diagonalization, takes the 2nd element of the first list, 3rd element of second list, etc. and creates a new unique list not in the stream

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f (fst (split n)) (snd (split n))

split :: Int -> (Int, Int)
split x
 | x - y * y >= y = (y, y * y + 2 *y - x)
 | otherwise = (x - y * y, y)
 where y = floor(sqrt (fromIntegral x))

-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n = False
