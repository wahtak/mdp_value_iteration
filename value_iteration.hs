-- implementation of MDP value iteration in Haskell
-- written by novice Haskell user (so don't expect well written code)
-- see: http://www.cs.cmu.edu/afs/cs/academic/class/15780-s16/www/slides/mdps.pdf

import Data.List
import Data.Ord

-- field of valid positions, all positions outside of field are invalid
validPos = [[ True,  True,  True,  True],
            [ True, False,  True,  True],
            [ True,  True,  True,  True]]

-- reward for being on position
rewards = [[   0.0,    0.0,    0.0,    1.0],
           [   0.0,    0.0,    0.0, -100.0],
           [   0.0,    0.0,    0.0,    0.0]]

-- probability distribution for each action, i.e. moves with probabilities
actions = [[(( 0, -1), 0.8), ((-1,  0), 0.1), (( 1,  0), 0.1)],
           [(( 0,  1), 0.8), ((-1,  0), 0.1), (( 1,  0), 0.1)],
           [((-1,  0), 0.8), (( 0, -1), 0.1), (( 0,  1), 0.1)],
           [(( 1,  0), 0.8), (( 0, -1), 0.1), (( 0,  1), 0.1)]]

actionLabels = ['<', '>', '^', 'v']

-- initial values of zeros
zeros nrow ncol = replicate nrow (replicate ncol 0.0)
initialValues = zeros (numRows validPos) (numCols validPos)

discountFactor = 0.9

getAt ls (y, x) = ls !! y !! x

numRows xss = length xss
numCols xss = length (head xss)

-- map a function to a 2d-list and pass coordinates to function
map2dCoords :: ([[a]] -> (Int, Int) -> b) -> [[a]] -> [[b]]
map2dCoords f a = [[f a (y, x) | x <- [0..(numCols a - 1)]] | y <- [0..(numRows a - 1)]]

-- check if position (y, x) is valid
isValidPos (y, x)
    | y < 0                 = False
    | y >= numRows validPos = False
    | x < 0                 = False
    | x >= numCols validPos = False
    | otherwise             = validPos !! y !! x

-- move is a tuple of position offsets (dy, dx)
applyMove (y, x) (dy, dx) = (y + dy, x + dx)

isValidMove pos move = isValidPos (applyMove pos move)

moveValue values pos move
    | isValidMove pos move = getAt values (applyMove pos move)
    | otherwise            = getAt values pos

actionValue values pos action = sum [moveValue values pos move * prob | (move, prob) <- action]

-- value iteration step
updatedValues :: [[Double]] -> [[Double]]
updatedValues values = map2dCoords updatedValue values

updatedValue :: [[Double]] -> (Int, Int) -> Double
updatedValue values pos
    | isValidPos pos = getAt rewards pos + discountFactor * maximum (map (actionValue values pos) actions)
    | otherwise      = 0.0

-- get best action for each value
bestActionLabels :: [[Double]] -> [[Char]]
bestActionLabels values = map2dCoords bestActionLabel values

bestActionLabel :: [[Double]] -> (Int, Int) -> Char
bestActionLabel values pos
    | isValidPos pos = snd (maximumBy (comparing fst) (zip (map (actionValue values pos) actions) actionLabels))
    | otherwise = ' '

-- iterate and add value to list for indices
-- compare with implementation of built-in iterate:
iterateIndices :: [Int] -> (a -> a) -> a -> [a]
iterateIndices indices f a = take (length indices) (iterateIndicesR 0 indices f a)

iterateIndicesR :: Int -> [Int] -> (a -> a) -> a -> [a]
iterateIndicesR i indices f a
-- recurse and add value to list
    | head indices == i = a : iterateIndicesR (i + 1) (tail indices) f (f a)
-- recurse without adding value to list
    | otherwise         = iterateIndicesR (i + 1) indices f (f a)

-- padL :: Int -> String -> String
padL n s
    | length s < n  = replicate (n - length s) ' ' ++ s
    | otherwise     = s

formatValues values = unlines [unwords [padL 10 (take 8 (show elem)) | elem <- row] | row <- values]

formatLabels labels = unlines [unwords [padL 2 [elem] | elem <- row] | row <- labels]

formatResults xs = map formatValues xs ++ [(formatLabels . bestActionLabels) (last xs)]

main = do
    putStrLn (unlines
        (formatResults
            (iterateIndices [0, 1, 2, 5, 10, 1000] updatedValues initialValues)))


-- output:
--
--       0.0        0.0        0.0        0.0
--       0.0        0.0        0.0        0.0
--       0.0        0.0        0.0        0.0
--
--       0.0        0.0        0.0        1.0
--       0.0        0.0        0.0     -100.0
--       0.0        0.0        0.0        0.0
--
--       0.0        0.0   0.720000       1.81
--       0.0        0.0        0.0     -99.91
--       0.0        0.0        0.0        0.0
--
--  0.809948   1.598952   2.475555   3.745858
--  0.268738        0.0   0.302045   -99.5921
--       0.0   3.359232   0.122238   4.199040
--
--  2.686009   3.527450   4.402477   5.812031
--  2.020696        0.0   1.095457   -98.8251
--  1.390107   0.903906   0.738328   0.123491
--
--  5.469982   6.313086   7.189904   8.668901
--  4.802911        0.0   3.346703   -96.6728
--  4.161489   3.653990   3.222062   1.526240
--
-- >  >  >  ^
-- ^     <  <
-- ^  <  <  v
