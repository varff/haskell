multMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
multMatrix [[a, b], [c, d]] [[e, f], [g, h]] = [[a * e + b * g, a * f + b * h], [c * e + d * g, c * g + d * h]]

sqMatrix :: Num a => [[a]] -> [[a]]
sqMatrix [[a, b], [c, d]] = [[a * a + b * c, a * b + b * d], [a * c + c * d, b * c + d * d]]

powMatrix :: Num a => Int -> [[a]] -> [[a]]
powMatrix 0 _ = [[1, 0], [0, 1]]
powMatrix 1 a = a
powMatrix p a = if mod p 2 == 1 then multMatrix a (powMatrix (pred p) a) else powMatrix (quot p 2) (sqMatrix a)

getFibNumber :: Num a => [[a]] -> a
getFibNumber [[_, _], [_, x]] = x

fib :: Int -> Int
fib ind = getFibNumber(powMatrix ind [[0, 1], [1, 1]])