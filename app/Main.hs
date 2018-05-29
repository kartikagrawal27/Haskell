--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Main where

main :: IO ()
main = return ()

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake 0 _  = []
mytake n [] = []
mytake n (x:xs) = if n<=0 
					then []
				  else
				  	x : (mytake (n-1) xs)

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs) = if n<=0
					then (x:xs)
				  else
				  	mydrop (n-1) xs



--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = revHelper [] (x:xs)
	where
		revHelper a [] = a
		revHelper a (x:xs) = revHelper (x:a) xs

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] a = a
app a [] = a
app (x:xs) y  = x : (app xs y)
--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0 
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] a = []
myzip a [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys 

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs a [] = []
addpairs [] a = []
addpairs (x:xs) (y:ys) = (x+y) : addpairs xs ys

--- ### ones: 

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a [] = [a]
add b (x:xs) =  if (b<x) 
					then b:x:xs
				else if b==x
					then x:xs
				else
					x:add b xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union a [] = a
union [] a = a
union (x:xs) (y:ys)  = if(x< y)
							then x : (union xs (y:ys))
					   else if (x>y)
					   	    then y : (union (x:xs) ys)
					   else
					   	    x : (union xs ys) 

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect a [] = []
intersect [] a = []
intersect (x:xs) (y:ys) = if x==y
							then x:(intersect xs ys)
						  else if x<y
						  	then intersect xs (y:ys)
						  else
						  	intersect (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = union b (map (x:) b)
                where b = powerset xs

powerHelper :: [a] -> [a] -> [a]
powerHelper [] b = b
powerHelper (x:xs) b = x : (powerHelper b xs)
--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = map (+1)

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = foldr (+) 0

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons (x) (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons a b) = a:(cons2list b) 

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp n) = n
eval (PlusExp []) = 0
eval (MultExp []) = 1
eval (PlusExp (x:xs)) = (eval x) + (eval (PlusExp xs))
eval (MultExp (x:xs)) = (eval x) * (eval (MultExp xs))

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' = foldr (\x y -> Cons x y) Nil
-- list2cons' [] = Nil
-- list2cons' (x:xs) = foldr 

data BinTree a = Node a (BinTree a) (BinTree a)
				| Leaf
	deriving(Show, Eq)

-- -- BinTree

-- -- - ### sumTree

-- -- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node x left right) = x + (sumTree left) + (sumTree right)

-- --- ### SimpVal

-- -- SimpVal
data SimpVal =	  IntVal Integer
				| BoolVal Bool
				| StrVal String
				| ExnVal String
			deriving (Show)

-- --- ### liftIntOp

-- -- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal a) (IntVal b) = IntVal (op a b)
liftIntOp _ _ _ = (ExnVal "not an IntVal!")

