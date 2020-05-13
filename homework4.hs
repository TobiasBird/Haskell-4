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

maxx :: Int -> Int -> Int
maxx x1 x2 = if x1>x2 then x1 else x2

maxy :: Int -> Int -> Int
maxy y1 y2 = if y1>y2 then y1 else y2

bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD s1 s2) = ((maxx x1 x2), y1 + y2)
				where (x1,y1) = bbox s1
					  (x2,y2) = bbox s2
bbox (LR s1 s2) = ((x1 + x2), maxy y1 y2)
				where (x1,y1) = bbox s1
					  (x2,y2) = bbox s2
