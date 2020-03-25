form :: [a] -> (Int, Int) -> [[a]] 
makeList :: [a] -> Int -> [a]

drop1 0 y = y          
drop1 _ [] = []
drop1 n (x:y) = (drop1 (n-1) y)

makeList (x:xs) 0 = []
makeList [] 0 = []
makeList (x:xs) y = x: (makeList (xs) (y-1))

form _ (0,0) = []
form _ (0,y) = []
form (lst) (x,y) = (makeList (lst) y) : (form (drop1 y (lst)) (x-1,y))

constGrid :: a -> (Int, Int) -> [[a]]
makeList2 :: a -> Int -> [a]
makeList2 a 0 = [] 
makeList2 a y = a: (makeList2 (a) (y-1))

constGrid a (0,0) = [[]]
constGrid a (0,y) = []
constGrid a (x,y) = (makeList2 a y) : constGrid a (x-1,y)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = (x) ++ (flatten xs)

access :: [[a]] -> (Int, Int) -> a 
access (xs) (width,height)  = xs !! width !! height

-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice_col :: Int -> Int -> [a] -> [a]
slice_col from to xs = take (to - from ) (drop from xs)

slice_row (lst) (i1,i2) (j1,j2) | (i1==i2) = []
slice_row (lst) (i1,i2) (j1,j2) =  (slice_col j1 j2 (lst !! i1)): (slice_row (lst) (i1+1,i2) (j1,j2))

slice (lst) (i1,i2) (j1,j2) = slice_row (lst) (i1,i2) (j1,j2)

vcat :: [[a]] -> [[a]] -> [[a]]

vcat (lst1) (lst2) = lst1 ++ lst2

hcat :: [[a]] -> [[a]] -> [[a]]

hcat [] [] = []
hcat (x:xs) (x1:xs1) = (x++x1) : (hcat (xs) (xs1))
without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]

remove_col :: Int -> Int -> [a] -> [a]
remove_row :: Int -> Int -> [[a]] -> [[a]]

remove_col 0 0 (x) = x
remove_col 0 to (x:xs) = remove_col 0 (to-1) (xs)
remove_col from to (x:xs) = x : remove_col (from-1) (to-1) (xs)

remove_c [] (j1,j2) = []
remove_c (x:xs) (j1,j2) = (remove_col j1 j2 (x)) : (remove_c (xs) (j1,j2))

remove_row 0 0 (x) = x
remove_row 0 to (x:xs) = remove_col 0 (to-1) (xs)
remove_row from to (x:xs) = x : remove_col (from-1) (to-1) (xs)

without (x:xs) (i1,i2) (j1,j2) = remove_c (remove_row i1 i2 (x:xs)) (j1,j2)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs

dimension :: [[a]] -> (Int, Int)
dimension listt = (length listt, length $ head listt)

matches2d grid pattern = 
    let lowerGrids = take (m-p+1) $ iterate tail grid
        rowMatches = map matches2dRight lowerGrids
     in concatMap (\(i, row) -> map (\j -> (i, j)) row) $ enumerate rowMatches
  where
    (m, n) = dimension grid
    (p, q) = dimension pattern
    matches2dRight grid = 
        let rightGrids = take (n-q+1) $ iterate (map tail) grid 
            matches = map topLeftMatches rightGrids
            indices = map fst $ filter snd $ enumerate matches --fst derives the first and snd gets the second item
         in indices 
    topLeftMatches g = and $ zipWith (\r1 r2 -> and $ zipWith (==) r1 r2) g pattern
    
main = print $ matches2d [[1,0,1,0,1],[0,1,0,1,0],[1,0,1,0,1],[0,1,0,1,0],[1,0,1,0,1]] [[0,1,0],[1,0,1],[0,1,0]] 
