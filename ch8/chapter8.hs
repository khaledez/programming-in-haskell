-- import Prelude

module Chapter8 where

type String = [Char]

type Pos = (Int, Int)
type Trans = Pos -> Pos


type Assoc k v = [(k, v)]

find :: Prelude.Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v ) <- t, k' == k]


data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y)  = (x+1, y)
move West (x,y)  = (x-1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East


data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y


data Maybe a = Nothing | Just a deriving Show

safediv :: Int -> Int -> Chapter8.Maybe Int
safediv _ 0 = Chapter8.Nothing
safediv m n = Chapter8.Just (m `div` n)

safehead :: [a] -> Chapter8.Maybe a
safehead [] = Chapter8.Nothing
safehead xs = Chapter8.Just (head xs)

-- 8.4 Recursive types
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)


data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs


-- 8.5 Class and instance declarations

class Eq a where
  (===), (/==) :: a -> a -> Bool

  x /== y = not (x === y)

instance Chapter8.Eq Bool where
  False === False = True
  True  === True  = True
  _ === _ = False


-- Exercises
-- 1. multiplication
mult :: Nat -> Nat -> Nat
mult (Succ (Succ Zero)) n = n
mult (Succ m) n = mult m (add n n)
