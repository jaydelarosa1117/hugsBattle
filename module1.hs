{-# LANGUAGE ParallelListComp #-}

mkBoard :: Int -> [[Int]]
mkBoard n = replicate n (replicate n 0)

isGameOver :: [[Int]] -> Bool
isGameOver board = checkList (concat board) where 
    checkList [] = True
    checkList (h:t) | h > 0 = False 
                    | otherwise = checkList t
                    
isPlaceable :: Int -> Int -> Int -> Bool -> [[Int]] -> Bool
isPlaceable n x y dir [] = False
isPlaceable n x y dir board | length board < n = False
                            | x < 0 = False 
                            | y < 0 = False 
                            | x > length board = False
                            | y > length board = False
                            | dir == True && (n+x) > length board = False
                            | dir == False && (n+y) > length board = False
                            | otherwise = isOccupied n x y dir board where
                                isOccupied n x y dir board | n == 0 = True
                                                           | (board !! y !! x) > 0 = False
                                                           | dir == True = isOccupied (n-1) (x+1) y dir board 
                                                           | dir == False = isOccupied (n-1) x (y+1) dir board
                                                           | otherwise = False
                                                    