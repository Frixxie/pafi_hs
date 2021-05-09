import           System.Random

data Node = Node Int Int
  deriving Show

new :: Int -> Int -> Node
new = Node

randNew :: RandomGen g => g -> Int -> Int -> Int -> Int -> Node
randNew gen minx maxx miny maxy =
  Node (fst $ randomR (minx, maxx) gen) (fst $ randomR (minx, maxx) gen)

genRandNodes :: RandomGen g => g -> Int -> Int -> Int -> Int -> Int -> [Node]
genRandNodes gen n minx maxx miny maxy =
  [ randNew gen minx maxx miny maxy | _ <- [0 .. n] ]

distanceTo :: Node -> Node -> Float
distanceTo (Node n1x n1y) (Node n2x n2y) = sqrt
  $ fromIntegral (((n1x - n2x) * (n1x - n2x)) + ((n1y - n2y) * (n1y - n2y)))

calcPath :: Node -> [Node] -> Float
calcPath head (x : y : ys) = distanceTo x y + calcPath head (y : ys)
calcPath head (x     : xs) = distanceTo x head
