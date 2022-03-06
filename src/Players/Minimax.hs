{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}

{-# LANGUAGE ParallelListComp #-}

module Players.Minimax where

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array
import Data.Char
import Data.Function

import Types
import Constants
import Cell
import Action
import Board
import Player
import Game
import Players.Dumb (dumbAction)


{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + (maximum (map (stateTreeDepth . snd) ts))

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- 
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]
generateGameTree :: Game -> GameTree
generateGameTree g
    | isFinished = StateTree g []
    | otherwise = StateTree g [ (x, generateGameTree y) | y <- gamePossibilities | x <- actionPossibilities]
    where
    gamePossibilities :: [Game]
    gamePossibilities = [ toJust x | x <- [performAction g x | x <- actionPossibilities] ]

    actionPossibilities :: [Action]
    actionPossibilities = validActions g

    isFinished :: Bool
    isFinished = or [hasWon x | x <- getPlayers g]

    getPlayers :: Game -> [Player]
    getPlayers (Game _ x) = x

    toJust :: Maybe a -> a
    toJust (Just a) = a



{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- Higher scoring nodes go first.
-- [Hint: You should use 'lowFirst'.]


highFirst :: StateTree Int a -> StateTree Int a
highFirst (StateTree v a) = StateTree v [(x, (highFirst y))| ( x , y ) <- (sortHigh a)]
    where
        sortHigh :: [(a, StateTree Int a)] -> [(a, StateTree Int a)]
        sortHigh x = sortOn (Down . getV) x

        getV :: (a , StateTree Int a ) -> Int
        getV (_ , StateTree v _ ) =  v

{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth 0 (StateTree v _)= StateTree v []
pruneDepth d (StateTree v a) = StateTree v [(fst x , pruneDepth (d - 1) (snd x) ) | x <- a]


{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]


pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth n (StateTree v a) = StateTree v [(x, (pruneBreadth n y))| ( x , y ) <- (take n a)]

{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]

utility :: Game -> Int
utility (game)
    | null (getPlayers game) = 0 --handling Nothing case for maybe
    | (cellCurrent game) `elem` winPos game = 200 --Large Score for being in winning position
    | null (reachableCells (getBoard game) (cellCurrent game)) = 0
    | otherwise = -1 + utility (bestGame game)

    where
        toJustGame :: Maybe Game -> Game
        toJustGame Nothing = Game (getBoard game) []
        toJustGame (Just (a)) = a

        cellCurrent :: Game -> Cell
        cellCurrent (Game _ x) = currentCell (currentPlayer x)

        winPos :: Game -> [Cell]
        winPos (Game _ x) = winningPositions (currentPlayer  x)

        listToSingle :: [a] -> a
        listToSingle [x] = x

        getBoard :: Game -> Board
        getBoard (Game x _) = x

        getPlayers :: Game -> [Player]
        getPlayers (Game _ x) = x

        bestGame :: Game -> Game
        bestGame game = toJustGame (performAction game (bestMove (cellCurrent game) (reachableCells (getBoard game) (cellCurrent game))))
            where
            bestMove :: Cell -> [Cell] -> Action
            bestMove x ys = Move (x, bestCell ys game)

            bestCell :: [Cell] -> Game -> Cell
            bestCell xs game = snd (minimumBy (comparing fst) [( distance x y, x) | x <- xs, validStep (getBoard game) (cellCurrent game, x), y <- winPos game])


            --Alternative to distance, but could not get it to work
            --numberSteps :: [Cell] -> Cell -> Cell -> Int   
            --numberSteps steps current winner | current == winner = length steps
            --                                 | length steps >= 10 = 999
            --                                 | otherwise = minimum [numberSteps (steps ++ [x]) x winner| x <- reachableCells (getBoard game) current]



-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree
evalTree = mapStateTree utility

{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]

--

minimaxFromTree :: EvalTree  -> Action
minimaxFromTree (StateTree v (x:xs) ) = fst (head (sortBy (flip compare `on` snd) [(x , minimax y ) | (x , y) <- x:xs]))

    where
        bestAction :: Result -> Action
        bestAction (Result score (x:xs)) = x

        minimax :: StateTree Int Action -> Result
        minimax evalTree = maximumP [] [] evalTree

        getResult :: [Int] -> [Action] -> Result
        getResult scores listActions = Result (sum scores) listActions

        maximumP :: [Int] -> [Action] -> EvalTree -> Result
        maximumP score listActions (StateTree v (x:xs) ) | null (x:xs) = getResult score listActions
                                                         | otherwise =  minimumP (score ++ [v]) (listActions ++ [fst x] ) (snd x)
        
        minimumP :: [Int] -> [Action] -> EvalTree -> Result
        minimumP score listActions (StateTree v (x:xs)) | null (x:xs) = getResult score listActions
                                                        | otherwise = maximumP (score ++ [v]) (listActions ++ [fst (last (x:xs))]) (snd (last(x:xs))) 

{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]

minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree (StateTree v (x:xs) ) = fst (head (sortBy (flip compare `on` snd) [(x , minimax y ) | (x , y) <- x:xs]))

    where
        bestAction :: Result -> Action
        bestAction (Result score (x:xs)) = x

        minimax :: StateTree Int Action -> Result
        minimax evalTree = maximumP 0 0 [] [] evalTree

        getResult :: [Int] -> [Action] -> Result
        getResult scores listActions = Result (sum scores) listActions

        maximumP :: Int -> Int -> [Int] -> [Action] -> EvalTree -> Result
        maximumP alpha beta score listActions (StateTree v (x:xs) ) | null (x:xs) = getResult score listActions
                                                                    | v <= alpha = getResult score listActions
                                                                    | otherwise =  minimumP v alpha (score ++ [v]) (listActions ++ [fst x]) (snd x)
        
        minimumP :: Int -> Int -> [Int] -> [Action] -> EvalTree -> Result
        minimumP alpha beta score listActions (StateTree v (x:xs) ) | null (x:xs) = getResult score listActions
                                                                    | v >= beta = getResult score listActions
                                                                    | otherwise = maximumP alpha v (score ++ [v]) (listActions ++ [fst (last (x:xs))]) (snd (last(x:xs)))

{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int
depth = 4

-- Given breadth for pruning.
breadth :: Int
breadth = 10

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax =
      minimaxFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadth
    . highFirst
    . evalTree
    . pruneDepth depth
    . generateGameTree

-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c,
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
