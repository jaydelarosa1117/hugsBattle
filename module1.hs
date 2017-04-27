{-# LANGUAGE ParallelListComp #-}

mkBoard :: Int -> [[Int]]
mkBoard n = replicate n (replicate n 0)

isGameOver :: [[Int]] -> Bool
isGameOver board = checkList (concat board) where 
    checkList [] = True
    checkList (h:t) | h > 0 = False 
                    | otherwise = checkList t
                    
isShipPlaceable :: Int -> Int -> Int -> Bool -> [[Int]] -> Bool
isShipPlaceable n x y dir [] = False
isShipPlaceable n x y dir board | length board < n = False
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


placeShip :: Int -> Int -> Int -> Bool -> Int -> Int -> Int -> [[Int]] -> [[Int]]
placeShip n x y dir currX currY size [] = [[]]
placeShip n x y dir currX currY size (h:t) | size == 0 = (h:t)
								| (dir == True) = (copyReplace n x 0 h): placeShip n (x+1) y dir (currX+1) currY (size-1) t
						        | (currY == y) = (copyReplace n x 0 h): placeShip n x (y+1) dir currX (currY+1) (size-1) t
						        | otherwise = h: placeShip n x (y+1) dir currX (currY + 1) (size) (t)


copyReplace :: Int -> Int -> Int -> [Int] -> [Int]
copyReplace size x currX [] = []
copyReplace size x currX (h:t) | (x==currX) = size: copyReplace size (x) (currX+1) (t)
							   | otherwise = h: copyReplace size x (currX+1) (t)
