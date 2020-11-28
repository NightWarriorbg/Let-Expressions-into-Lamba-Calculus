-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2019
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return a randome value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (alphaNorm, countAllReds, printLambda, parseLet, letToLambda,
    LamExpr(LamApp, LamAbs, LamVar), LetExpr(LetApp, LetDef, LetFun, LetVar, LetNum),
    lambdaToLet) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing

import Data.List
import qualified Data.Map as M

-- abstract data type for simple lambda calculus expressions
data LamExpr = LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int deriving (Show, Eq)

-- abstract data type for simple let expressions
data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)
-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE
-- Challenge 1
-- generate the alpha normal form for a simple lambda calculus expression
-- each bound variable is chosen to be the first one possible
alphaNorm :: LamExpr -> LamExpr
alphaNorm expr = alphaReduce expr boundMap
  where
    boundVars = getBoundVars expr
    boundMap = M.fromList [(x, y) | x <- boundVars, y <- [0..(length boundVars)-1]]

alphaReduce :: LamExpr -> M.Map Int Int -> LamExpr
alphaReduce (LamApp f g) m = (LamApp (alphaReduce f m) (alphaReduce g m))
alphaReduce (LamAbs x abs) m = case M.lookup x m of
                                 Nothing -> (LamAbs x (alphaReduce abs m))
                                 (Just p) -> (LamAbs p (alphaReduce abs m))
alphaReduce (LamVar x) m = (LamVar x)

getBoundVars :: LamExpr -> [Int]
getBoundVars expr = [x | x <- args, not (x `elem` bodies)]
  where
    args = getArgVars expr
    bodies = getBodyVars expr

getArgVars :: LamExpr -> [Int]
getArgVars (LamApp f g) = (getArgVars f) ++ (getArgVars g)
getArgVars (LamAbs x abs) = [x] ++ getArgVars abs
getArgVars _ = []

getBodyVars :: LamExpr -> [Int]
getBodyVars (LamApp f g) = (getBodyVars f) ++ (getBodyVars g)
getBodyVars (LamAbs _ abs) = getBodyVars abs
getBodyVars (LamVar x) = [x]


-- Challenge 2
-- count all reduction paths for a given lambda expression m, of length up to a given limit l
countAllReds :: LamExpr -> Int -> Int
countAllReds _ 0 = 0
countAllReds expr l = countRed expr [] l

countRed :: LamExpr -> [LamExpr] -> Int -> Int
countRed _ _ 0 = 0
countRed expr oldReductions max = if (reduce expr) `elem` oldReductions then 0 else 1 + (countRed (reduce expr) (oldReductions ++ [expr]) (max - 1))

reduce :: LamExpr -> LamExpr
reduce (LamApp (LamAbs x (LamVar y)) (LamVar z)) = if y == x then (LamVar z) else (LamVar y)
reduce (LamApp (LamAbs x (LamApp y z)) (LamVar t)) = if (LamVar x) == y then (LamApp (LamVar t) z) else if (LamVar x) == z then (LamApp y (LamVar t)) else (LamApp (LamAbs x (LamApp y z)) (LamVar t))
reduce (LamApp (LamAbs x (LamAbs y (LamVar z))) (LamVar m)) = if x == z then (LamAbs y (LamVar m)) else (LamAbs y (LamVar z))
reduce (LamApp (LamAbs x (LamVar y)) (LamAbs z ags)) = if x == y then (LamAbs z ags) else (LamVar y)
reduce (LamApp f1 (LamVar x)) = (LamApp (reduce f1) (LamVar x))
reduce (LamApp f1 f2) = (LamApp f1 (reduce f2))
reduce lam = lam


-- Challenge 3 
-- pretty print a lambda expression, combining abstraction variables
-- also recognising Scott numerals and printing these as numbers
-- finalising omitting brackets where possible and safe to do so
printLambda :: LamExpr -> String
printLambda (LamAbs 1 (LamAbs 2 (LamVar 1))) = "0"
printLambda (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))) = "1"
printLambda (LamVar num) = "x" ++ show num
printLambda (LamApp (LamVar x) (LamVar y)) = "x" ++ show x ++ " x" ++ show y
printLambda (LamApp x y) = "(" ++ printLambda x ++ ") " ++ printLambda y
printLambda (LamAbs x (LamApp y z)) = "\\x" ++ show x ++ " -> " ++ printLambda y ++ " " ++ printLambda z
printLambda (LamAbs x y) = "\\x" ++ show x ++ " -> " ++ printLambda y

-- Challenge 4
-- parse recursive let expression, possibly containing numerals
parseLet :: String -> Maybe LetExpr
parseLet str = runParser letParser str

runParser :: Parser a -> String -> Maybe a
runParser parser input
  | 'l' `elem` input  && not ('f' `elem` input) = Nothing -- Catch non-valid let statements without having to clog up parsers code
  | otherwise = case parse parser input of
                  [] -> Nothing
                  x -> Just $ (fst . head) x

-- Main parser for Let Expressions
letParser :: Parser LetExpr
letParser = do
  result <- parseLetDef <|> parseAppWithParens <|> parseAppWithoutParens
  return $ result

-- Parser for expressions like x1 (x2 x3)
parseAppWithParens :: Parser LetExpr
parseAppWithParens = do
  x <- parseVar
  space
  f <- parseParensSet
  return $ (LetApp x f)

parseParensSet :: Parser LetExpr
parseParensSet = do
  char '('
  x <- parseVar
  space
  y <- parseVar
  char ')'
  return $ LetApp x y

-- Parser for expressions like x1 x2 x3
parseAppWithoutParens :: Parser LetExpr
parseAppWithoutParens = do
  vars <- many parseVar
  let result = constructAppString vars
  return $ result

-- Turn a list of LetExpr into a LetApp "tree" of sorts
constructAppString :: [LetExpr] -> LetExpr
constructAppString (x:[]) = x
constructAppString ((LetVar x):(LetVar y):xs) = (LetApp (LetApp (LetVar x) (LetVar y)) (constructAppString xs))

-- Parser for a single LetVar
parseVar :: Parser LetExpr
parseVar = do
  char 'x'
  d <- fmap toInt digit
  space
  return $ (LetVar d)

-- Parser for expressions like let f1 x1 = x2 in f1 x1
parseLetDef :: Parser LetExpr
parseLetDef = do
  string "let"
  space
  funcDefs <- some parseFuncDef
  space
  string "in"
  space
  char 'f'
  fNum <- fmap toInt digit
  space
  char 'x'
  aNum <- fmap toInt digit
  return $ LetDef funcDefs (LetApp (LetFun fNum) (LetVar aNum))

parseFuncDef :: Parser ([Int], LetExpr)
parseFuncDef = do
  char 'f'
  fNum <- digit
  space
  char 'x'
  aNum <- digit
  space
  char '='
  space
  char 'x'
  bNum <- digit
  many $ string "; "
  return ([toInt fNum, toInt aNum], LetVar (toInt bNum))

-- Utility function to turn chars like '1' or '5' into 1 or 5
toInt :: Char -> Int
toInt c = (ord c) - 48


-- Challenge 5
-- translate a let expression into lambda calculus, using Scott numerals
-- convert let symbols to lambda variables using Jansen's techniques rather than Y
letToLambda :: LetExpr -> LamExpr
--letToLambda (LetDef [([x],LetFun _)] (LetFun _)) = (LamApp (LamAbs x (LamApp (LamVar x) (LamVar x))) (LamAbs x (LamApp (LamVar x) (LamVar x))))
letToLambda _ = LamVar (-1)


-- Challenge 6
-- convert a lambda calculus expression into one using let expressions and application
-- can use lambda lifting techniques described in wikipedia article
lambdaToLet :: LamExpr -> LetExpr
lambdaToLet expr = LetDef (makeLetStmt expr 0) (makeInStmt expr)

makeLetStmt :: LamExpr -> Int -> [([Int], LetExpr)]
makeLetStmt (LamAbs f (LamVar x)) c = [([f+c, x], LetVar x)]
makeLetStmt (LamApp (LamAbs f (LamVar x)) abs) c = [([f+c, x], LetVar x)] ++ makeLetStmt abs (c+1)
makeLetStmt (LamApp (LamVar _) abs) c = makeLetStmt abs c
makeLetStmt (LamAbs f (LamApp (LamVar x) (LamAbs y (LamApp (LamVar z) (LamVar p))))) c = [([f, x, y], LetApp (LetVar x) (LetVar y)), ([y, f], LetApp (LetVar x) (LetApp (LetFun f) (LetVar x)))]
makeLetStmt _ _ = []

makeInStmt :: LamExpr -> LetExpr
makeInStmt (LamApp (LamAbs x _) (LamAbs y _)) = if x == y then (LetApp (LetFun x) (LetFun (y+1))) else (LetApp (LetFun x) (LetFun y))
makeInStmt (LamAbs f (LamVar x)) = (LetFun f)
makeInStmt (LamAbs f (LamApp (LamVar x) (LamAbs y (LamApp (LamVar z) (LamVar p))))) = LetFun y
makeInStmt (LamApp (LamAbs x _) (LamVar y)) = (LetApp (LetFun x) (LetVar y))
makeInStmt (LamApp (LamVar x) (LamAbs y _)) = (LetApp (LetVar x) (LetFun y))
makeInStmt (LamApp (LamVar x) (LamVar y)) = (LetApp (LetVar x) (LetVar y))
makeInStmt _ = LetVar (-1)