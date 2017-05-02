module Main where
	
import System.IO 
import System.Random
import System.IO.Unsafe
import Board

rand :: Bool -> Int
rand x = unsafePerformIO randomIO

generateRandom :: Int -> Int
generateRandom n = ((mod (rand True) n))

placeShips [] board = board
placeShips (h:t) board = do 
						 if isShipPlaceable h x y (dir == 1) board
						 then placeShips t (placeShip h x y (dir==1) board)
						 else placeShips (h:t) board where
						 {x = generateRandom 11; y = generateRandom 11; dir = generateRandom 2}


getXY board =	do
       putStrLn "Enter a positive x value"
       line <- getLine
       let parsed = reads line :: [(Int, String)] in
         if length parsed == 0
         then getX' board
         else let (x, _) = head parsed in
           if x >= 0 
           then do
       			putStrLn "Enter a positive y value"
       			line <- getLine
       			let parsed = reads line :: [(Int, String)] in
         		  if length parsed == 0
         		  then getX' board
         		  else let (y, _) = head parsed in
           		  	if y >= 0 
           		  	then (boardToStr sqToStr (hitBoard x y board))
           		  	else getX' board
           else getX' board
       where
         getX' board = do
           putStrLn "Invalid input!"
           getXY board

main = getXY (placeShips [5,4,3,2,2] (mkBoard 10))
