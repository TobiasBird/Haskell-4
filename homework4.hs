-- Group Members: Harper Swenson, Tobias Bird, Victor Campa, Gregory Oertli --
--
-- exercise 1 --

type Prog = [Cmd]
data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int

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
rankC DUP = (0,1) -- does not take off the stack (?)
rankC INC = (1,1)
rankC SWAP = (2,2)
rankC (POP k) = (k,k) -- removes however many listed

-- Call rankC like so ... rankC (LD 4) .. or .. rankC ADD .. or .. rankC (POP 3)

-- Having a hard time figuring out the right way to do this.
rankP :: Prog -> Maybe Rank
rankP something = (rank something 0)

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (x:xs) r = if (Just (getRank (rankC x)) >= 0) then (Just ( (r + getRank (rankC x)) + (rank xs r)) ) else (Nothing)

getRank :: CmdRank -> Rank
getRank (x,y) =  (y-x)

-- exercise 2 --

data Shape = X
	   | TD Shape Shape
	   | LR Shape Shape
	   deriving Show

type BBox = (Int,Int)

-- a --
--data Type = Int
	    --deriving (Eq, Show)
-- bbox :: Shape -> BBox
-- bbox X = (1,1)
-- bbox (TD s1 s2) | bbox s1==BBox && bbox s2==BBox = BBox
-- bbox (LR s1 s2) | bbox s1==BBox && bbox s2==BBox = BBox
