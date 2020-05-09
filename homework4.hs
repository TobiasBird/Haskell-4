-- Group Members: Harper Swenson, Tobias Bird, Victor Campa, Gregory Oertli --
--
-- exercise 1 --


-- exercise 2 --
data Shape = X
	   | TD Shape Shape
	   | LR Shape Shape
	   deriving Show

type BBox = (Int,Int)

-- a --
--data Type = Int
	    --deriving (Eq, Show)
bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD s1 s2) | bbox s1==BBox && bbox s2==BBox = BBox
bbox (LR s1 s2) | bbox s1==BBox && bbox s2==BBox = BBox
