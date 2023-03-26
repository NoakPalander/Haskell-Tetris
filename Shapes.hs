module Shapes where

import Data.List (transpose)
import Data.Maybe (isJust)
import Test.QuickCheck

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape { rows :: [Row] } deriving Eq

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
 where
  showRow r = [showSquare s | s <- r]
    
  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes] 
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- Part A1:

-- Constructs an empty shape of a given dimension
emptyShape :: (Int, Int) -> Shape
emptyShape (r, c) = Shape $ replicate c emptyRow
  where
    emptyRow = replicate r Nothing

-- Part A2:

-- Returns the given column and row size of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize shape  = (c, r)
  where
    -- The transpose turns columns into rows, so we can use `length`
    c = length $ transpose $ rows shape
    r = length $ rows shape

-- Part A3:

-- Flattens the shape to 1D and counts `Just` elements
blockCount :: Shape -> Int
blockCount shape = length $ filter isJust $ concat $ rows shape

-- Part A4:

-- Property for Shape
prop_Shape :: Shape -> Bool
prop_Shape shape = isRect && c > 0 && r > 0
  where
    -- Length of the first row
    first = length $ (rows shape) !! 0

    -- Checks if all rows are the same length as the first
    isRect = all (\x -> length x == first) $ rows shape

    -- Size of the shape
    (c, r) = shapeSize shape


-- Part A5:

-- Picks a random color from the list of all Colour's
genColour :: Gen Colour
genColour = elements [minBound..maxBound]

instance Arbitrary Colour where
  arbitrary = genColour

-- Part A6:

-- Picks a random shape from all shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- Part A7:

-- Rotates a shape 90°
-- Reversing a matrix and transposing results in a 90° CW rotation of each row
rotateShape :: Shape -> Shape
rotateShape = Shape . transpose . reverse . rows

-- Rotates a shape CCW 90°
-- The direct inverse of `rotateShape`
rotateShapeCounter :: Shape -> Shape
rotateShapeCounter = Shape . reverse . transpose . rows

-- Part A8:

-- Shifts a shape downwards by padding above it with `Nothing` recursively
-- If we try to shift by a negative amount or 0 we just return the shape
shiftV :: Int -> Shape -> Shape
shiftV n s
  | n <= 0    = s
  | otherwise = Shape $ (replicate c Nothing) : rows (shiftV (n - 1) s)
  where
    (c, _) = shapeSize s

-- Shifts a shape to the right by adding padding to the left with `Nothing`
-- This is equivalent to first rotating 90° CCW, padding above it, and rotating it back
shiftH :: Int -> Shape -> Shape
shiftH n shape = rotateShapeCounter $ shiftV n $ rotateShape shape

-- Shifts a shape both horizontally and vertically
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (offsetC, offsetR) shape = shiftH offsetC $ shiftV offsetR shape

-- Part A9:

-- Adds padding underneath the shape with `Nothing`
-- This is equivalent to rotating it 90° CCW, shifting it horizontally, and rotating it back
padV :: Int -> Shape -> Shape
padV n shape = rotateShapeCounter $ shiftH n $ rotateShape shape

-- Adds padding to the right of the shape with `Nothing`
-- This is equivalent to rotating it 90° CW, shifting it vertically, and rotating it back
padH :: Int -> Shape -> Shape
padH n shape = rotateShape $ shiftV n $ rotateShapeCounter shape

-- Adds padding to a shape horizontally and vertically
padShape :: (Int, Int) -> Shape -> Shape
padShape (offsetC, offsetR) shape = padH offsetC $ padV offsetR shape

-- Part A10:

-- Pads a shape to a desired size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (targetC, targetR) s = padShape (targetC - c, targetR - r) s
  where (c, r) = shapeSize s

---------- Part B ----------

-- * Comparing and combining shapes
-- ** B1

-- | Test if two shapes overlap, recursively
overlaps :: Shape -> Shape -> Bool
overlaps _ (Shape []) = False
overlaps (Shape []) _ = False
overlaps (Shape (r1:r1s)) (Shape (r2:r2s))
  | rowsOverlap r1 r2 = True
  | otherwise = overlaps (Shape r1s) (Shape r2s)
  where 
    -- Test if two rows overlap
    rowsOverlap :: Row -> Row -> Bool
    rowsOverlap r1 r2 = or $ zipWith (\x y -> isJust x && isJust y) r1 r2

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
-- Uses pattern matching to extract the first row in each shape
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith _ s (Shape []) = s
zipShapeWith _ (Shape []) s = s
zipShapeWith func (Shape (r1:r1s)) (Shape (r2:r2s)) = Shape $ head' : tail'
  where 
    -- First row, zipped
    head' = zipWith func r1 r2
    -- Remaining rows, zipped recursively
    tail' = rows $ zipShapeWith func (Shape r1s) (Shape r2s)
    
-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
combine s1 s2 = zipShapeWith combineSquares (padShapeTo sz s1) (padShapeTo sz s2)
  where
    (w1, h1) = shapeSize s1     -- Extract size from s1
    (w2, h2) = shapeSize s2     -- Extract size from s2
    sz = (max w1 w2, max h1 h2) -- Minimum size to fit s1 and s2

    -- Combines two squares 
    combineSquares :: Square -> Square -> Square
    combineSquares Nothing (Just c) = Just c
    combineSquares (Just c) Nothing = Just c
    combineSquares Nothing Nothing = Nothing
    combineSquares _ _ = error "Cannot combine two non-empty squares"
