module Main where

import ConsoleGUI
-- import ThreepennyGUI  -- either use ConsoleGUI or ThreepennyGUI

import Shapes
import Data.List
import Data.Maybe

--------------------------------------------------------------------------------
-- * The code that puts all the piece together
main :: IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Pos   = (Int, Int)

-- | The state of the game consists of three parts:
data Tetris = Tetris
  { piece  :: (Pos, Shape)  -- ^ The position and shape of the falling piece
  , well   :: Shape         -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]       -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellWidth, wellHeight :: Int
wellWidth  = 10
wellHeight = 20

wellSize :: (Int, Int)
wellSize   = (wellWidth, wellHeight)

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Pos addition
add :: Pos -> Pos -> Pos
(x1, y1) `add` (x2, y2) = (x1 + x2, y1 + y2)

-- | Move the falling piece into position
place :: (Pos, Shape) -> Shape
place (v, s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_, p) w _) = prop_Shape p && shapeSize w == wellSize

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls (Shape r) = Shape $ [fullRow] ++ sides ++ [fullRow]
  where
    (w, _) = shapeSize $ Shape r -- Extract width from shape
    fullRow = replicate (w + 2) $ Just Black -- A full horizontal wall for the bottom and top
    sides = map (\x -> [Just Black] ++ x ++ [Just Black]) r -- The shape with added vertical walls

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls . combine w $ shiftShape v p

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  randIndex r = floor (r * fromIntegral (length allShapes))
  piece:supply = map (\r -> allShapes !! randIndex r) rs


-- Double -> Shape
-- 

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t = tick t
stepTetris MoveDown t = tick t
stepTetris MoveLeft t = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris Rotate t = Just (0, rotatePiece t)

-- B7
type Vector = (Int, Int)

-- Offset current falling piece by a vector
move :: Vector -> Tetris -> Tetris
move (dx, dy) (Tetris ((x, y), s) w ss) = Tetris ((x + dx, y + dy), s) w ss

-- B8
-- Make the falling piece fall
tick :: Tetris -> Maybe (Int, Tetris)
tick t
  | collision newT = dropNewPiece t
  | otherwise = Just (0, newT)
  where
    newT = move (0, 1) t

-- Part C
-- C1

collision :: Tetris -> Bool
collision (Tetris ((x, y), piece) well _)
  | x < 0 = True
  | x + w > fst wellSize = True
  | y + h > snd wellSize = True
  | overlaps well $ place ((x, y), piece) = True
  | otherwise = False
  where
    (w, h) = shapeSize piece

-- C3

movePiece :: Int -> Tetris -> Tetris
movePiece dx t
  | collision newT = t
  | otherwise = newT
  where
    newT = move (dx, 0) t

-- C4
rotate :: Tetris -> Tetris
rotate (Tetris (pos, piece) well shapes) = Tetris (pos, rotateShape piece) well shapes

-- C6
rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision newT = t
  | otherwise = newT
  where
    newT = rotate t

-- C7
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris ((x, y), piece) well (s:ss))
  | collision newT = Nothing
  | otherwise = Just (n, newT)
  where
    (n, clearedWell) = clearLines (combine well $ place ((x, y), piece))
    newT = Tetris (startPosition, s) clearedWell ss

dropNewPiece _ = error "Invalid state" -- Match an empty list of shapes

-- C9
isComplete :: Row -> Bool
isComplete r = not $ any isNothing r

clearLines :: Shape -> (Int, Shape)
clearLines (Shape rs) = (numCleared, Shape rs')
  where
    rsZipped = zip rs $ map isComplete rs
    numCleared = length $ filter snd rsZipped
    emptyRw = replicate (fst wellSize) Nothing

    rs' = replicate numCleared emptyRw ++ map fst (filter (not . snd) rsZipped)
