sumsqreven :: [Int] -> Int
sumsqreven ns = (filter even ns) . (map (^2)) . sum 

