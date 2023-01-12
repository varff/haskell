delannoyStep :: [Int] -> [Int] -> [Int]
delannoyStep cs ds = 1: zipWith3 (\x y z -> x + y + z) ds (tail ds) cs ++ [1]

helper :: [Int] -> [Int] -> [[Int]]
helper cs ds = cs: helper ds (delannoyStep cs ds)

delannoyLayers :: [[Int]]
delannoyLayers = helper [1] [1, 1]

delannoy :: Int -> Int -> Int
delannoy 0 _ = 1
delannoy _ 0 = 1
delannoy a b = last (take (a + b + 1) delannoyLayers) !! b