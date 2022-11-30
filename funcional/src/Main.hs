-- INICIO

main = do
   print $ transposta [[1,2,3],[4,5,6],[7,8,9]]

--1 ----------------------------------------

pkl :: [a] -> Integer -> [a]
pkl [] _ = []
pkl (x:xs) i
   | i > 0  = (x:pkl xs (i-1))
   | i == 0 = []
   | i < 0 = []

-- 2 ---------------------------------------

cpkl :: [a] -> Integer -> [a]
cpkl [] _ = []
cpkl (x:xs) i
   | i > 0  = cpkl xs (i-1)
   | i == 0 = (x:xs)

-- 3 ---------------------------------------
-- Não vale para tipos diferentes

emi :: Ord a => [a] -> a -> [a]
emi [] _ = []
emi (x:xs) i
   | x > i = (x:emi xs i)
   | x <= i = emi xs i

-- 4 ---------------------------------------

pl :: [Integer] -> Integer
pl [] = 0
pl (x:xs)
   | xs /= []  = x * pl xs
   | xs == []  = x

-- 5 ---------------------------------------

max_int :: [Integer] -> Integer
max_int [] = error "Lista vazia"
max_int (x:[]) = x
max_int (x:(y:ys))
   | y > x = max_int(y:ys)
   | otherwise = max_int(x:ys)

-- 6 ---------------------------------------

-- 7 ---------------------------------------

conc_pares :: [a] -> [b] -> [(a,b)]
conc_pares [] _ = []
conc_pares (x:xs) (y:ys) = [(x,y)] ++ conc_pares xs ys

-- 8 ---------------------------------------

desmembra_lista :: [(a,b)] -> ([a], [b])
desmembra_lista [] = ([], [])
desmembra_lista ((x,y): z) = (x:(fst resto), y:(snd resto))
   where resto = desmembra_lista z

-- 9 ---------------------------------------

replica :: Integer -> [a] -> [[a]]
replica 0 _ = []
replica i l = [l] ++ replica (i-1) l

-- 10 --------------------------------------

transposta :: [[a]] -> [[a]]
transposta ([]:_) = []
transposta x = (map head x) : transposta (map tail x)