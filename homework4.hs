-- Group Members: Harper Swenson, Tobias Bird, Victor Campa, Gregory Oertli --
--
-- exercise 1 --

type Prog = [Cmd]
data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int deriving (Eq, Show)

-- a --
type Rank = Int
type CmdRank = (Int, Int)

-- (n,m)
-- n is the number of elements the operation takes from the stack
-- m is the number of elments the operation puts on the stack
rankC :: Cmd -> CmdRank
rankC (LD _) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2) -- does not take off the stack (?)
rankC INC = (1,1)
rankC SWAP = (2,2)
rankC (POP k) = (k,0) -- removes however many listed

-- Call rankC like so ... rankC (LD 4) .. or .. rankC ADD .. or .. rankC (POP 3)

-- Having a hard time figuring out the right way to do this.
rankP :: Prog -> Maybe Rank
rankP something = rank something 0

getRank :: CmdRank -> Int
getRank (x,y) = y-x

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (x:xs) r = if (r + getRank (rankC x)) > 0 then rank xs (r +(getRank (rankC x))) else Nothing

-- b --

type Stack = [Int]
type D = Stack -> Stack

sem :: Prog -> D
sem [] xs = xs
sem (x:xs) a = sem xs (semCmd x a)

semCmd :: Cmd -> D
semCmd (LD a) xs = a : xs
semCmd ADD (a:b:xs) = (a+b) : xs
semCmd MULT (a:b:xs) = (a*b) : xs
semCmd DUP (a:xs) = a : a : xs
semCmd INC (a:xs) = succ a : xs
semCmd SWAP (a:b:xs) = b : a : xs
semCmd (POP a) xs = drop a xs

typeCorrect :: Prog -> Bool
typeCorrect e = rankP e /= Nothing

semStatTC :: Prog -> Maybe Stack
semStatTC e | typeCorrect e = Just (sem e []) | otherwise  = Nothing

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
				  where 
					  (x1,y1) = bbox s1
					  (x2,y2) = bbox s2
bbox (LR s1 s2) = ((x1 + x2), (maxy y1 y2))
				  where 
					  (x1,y1) = bbox s1
					  (x2,y2) = bbox s2

-- b --
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD s1 s2) = if x1==x2 then Just (x1,y1+y2) else Nothing
				where
					(x1,y1) = bbox s1
					(x2,y2) = bbox s2
rect (LR s1 s2) = if y1==y2 then Just (x1+x2,y1) else Nothing
				where
					(x1,y1) = bbox s1
					(x2,y2) = bbox s2
         