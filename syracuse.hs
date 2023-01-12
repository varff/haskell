collatz 1 = 0
collatz a
    | even a    = (+1) $ collatz $ div a 2
    | otherwise = (+1) (3 * a + 1)


Helper a n
    | a == 1    = n
    | even a    = Helper (div a 2) (n + 1)
    | otherwise = Helper (3*a + 1) (n + 1)


syracuse a
    | a == 1    = []
    | even a    = a : syracuse (div a 2)
    | otherwise = a : syracuse (3*a + 1)