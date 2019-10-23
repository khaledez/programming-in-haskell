grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]


replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [0..(n-1)]]


pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


factors :: Int -> [Int]
factors n = [x | x <- [1..(n-1)], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]


find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [i | i <- find x (zip xs [0..])]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
