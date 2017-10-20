-- pattern matching

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven"
lucky _ = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


captical :: String -> String
captical "" = "Empty string, whoops!"
captical all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- case
head' :: [a] -> a
head' [] = error "No head for empty list!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty list!"
                       (x:_) -> x
