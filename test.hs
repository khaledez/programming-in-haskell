double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns


qsort [] = []
qsort (x:xs) = qsort  larger ++ [x] ++ qsort smaller
                where
                  smaller = [a | a <- xs, a < x]
                  larger  = [b | b <- xs, b > x]


prod [] = 1
prod (x:xs) = x * prod xs

lastc xs = xs !! ((length xs) - 1)


initc xs = take (length xs - 1) xs

seqn []         = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

