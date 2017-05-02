module Main where
	
import System.IO 
import System.Random
import System.IO.Unsafe
import Board

placeShips :: [[Int]] -> [[Int]] -> [[Int]]
placeShips [] board = board
placeShips (h:t) board = do
							x <- randomRIO(1,10)
						    y <- randomRIO(1,10)
						    dir <- randomRIO(1,2)
							if isShipPlaceable h x y (dir==1) board 
							then placeShips t (placeShip h x y (dir==1) board))
					    	else placeShips (h:t) board
							
getXY board =	do
       putStrLn "Enter a positive x value?"
       line <- getLine
       let parsed = reads line :: [(Int, String)] in
         if length parsed == 0
         then getX' board
         else let (x, _) = head parsed in
           if x >= 0 
           then do
       			putStrLn "Enter a positive y value?"
       			line <- getLine
       			let parsed = reads line :: [(Int, String)] in
         		  if length parsed == 0
         		  then getX' board
         		  else let (y, _) = head parsed in
           		  	if y >= 0 
           		  	then print $ hitBoard x y board
           		  	else getX' board
           else getX' board
       where
         getX' board = do
           putStrLn "Invalid input!"
           getXY board