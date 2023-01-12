helper :: Int -> Int -> Int -> Int
helper a 0 d = d
helper a n d =
  if odd n
  then helper a (n - 1) (d * a)
  else helper (a*a) (n `div` 2) p

pow :: Int -> Int -> Int
pow a b = Helper a b 1