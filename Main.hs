module Main where

import UserInterface
import Database

main = do
	--f <- readFile "db"
	db <- welcome []
	putStrLn (show db) 