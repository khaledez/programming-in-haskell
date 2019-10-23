halve :: [a] -> ([a],[a])
halve lst = (take (length(lst)`div`2) lst, drop (length(lst)`div`2) lst)

myabs n | n >= 0 = n
        | otherwise = -n

mynot :: Bool -> Bool
mynot False = True
mynot True  = False


third1 :: [a] -> a
third1 (_:xs) = head (tail xs)

third2 :: [a] -> a
third2 [_,_,a] = a
third2 (_:(_:a:_)) = a

third3 :: [a] -> a
third3 xs = xs !! 2


safetail :: [a] -> [a]
safetail xs | null xs = []
            | otherwise = tail xs

safetailc :: [a] -> [a]
safetailc xs = if null xs then [] else tail xs

safetailp :: [a] -> [a]
safetailp [] = []
safetailp (_:xs) = xs


(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) _ True = True
(||) _ _ = False


kand :: Bool -> Bool -> Bool
kand a b = if a then b else a

mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

lmutl = \x -> (\y -> (\z -> x * y * z))


luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (luhnDouble a + b + luhnDouble c + d) 10 == 0 
