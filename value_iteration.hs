-- implementation of MDP value iteration in Haskell
-- written by novice Haskell user (so don't expect well written code)

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
--    iterate :: (a -> a) -> a -> [a]
--    iterate f a = a : iterate f (f a)
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
