module UserInterface where

import Database

welcome :: IO ()
welcome = do
	putStrLn "\nWelcome to the Phonebook interactive user interface."
	interaction db
		where
			db = [("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien","000 - 1111111"), ("Hauke Heien +X","000 - 1111112341"), ("Hauke Heien","000 - 1111111")]

interaction :: DB -> IO ()
interaction db = do
	putStrLn "Now we need you to enter a command (i|e|r|p|q|h) with 'h' for help"
	i <- readChar
	match i db

match :: Char -> DB -> IO ()
match i db
	| i == 'i' = insertio db
	| i == 'e' = editio db
	| i == 'r' = removeio db
	| i == 'p' = printio db
	| i == 'q' = quitio
	| i == 'h' = helpio db
	| otherwise = messio db

insertio :: DB -> IO ()
insertio db = do
	putStrLn "\nPlease specify the entry you want to add:"
	entry <- buildEntry
	putStrLn ""
	interaction (addtoDB entry db)

editio :: DB -> IO ()
editio db = do
	putStrLn "editing"
	interaction db

removeio :: DB -> IO ()
removeio db = do
	putStrLn "removing"
	interaction db

printio :: DB -> IO ()
printio db = do
	putStrLn ("\nID" ++ (whitespace (spacetid1 db)) ++ "Name" ++ (whitespace (spacetid2 db))++ "Number")
	printEntries  db db 0
	putStrLn ""
	interaction db

quitio :: IO ()
quitio = do
	putStrLn "\nBye, thanks for using the product."
	return ()

helpio :: DB -> IO ()
helpio db = do
	putStrLn "\nThis is an easy to use commandline interface which relies on soley capitals. These are the full versions of the words:\n"
	putStrLn "  i(nsert)   Add an item to the DB"
	putStrLn "  e(dit)     Edit one item"
	putStrLn "  r(emove)   Remove one item"
	putStrLn "  p(rint)    Print the whole DB"
	putStrLn "  q(uit)     Quit the appication"
	putStrLn "  p(rint)    Print this help\n"
	interaction db

messio :: DB -> IO ()
messio db = do
	putStrLn "You clearly don't know how to use the interface. I strongly encourage you to use the h(elp)."
	interaction db

printEntries :: DB -> DB -> Int -> IO ()
printEntries _ [] _ = return ()
printEntries db (e:es) i
	| i >= 0     = do
		printEntry i (spacetab1 db i) (spacetab2 db (getNumber e)) e
		printEntries db es (i+1)
	| otherwise = error "editEntryByID: Negative ID"

printEntry :: Int -> Int -> Int -> Entry -> IO ()
printEntry i sp1 sp2 (name, number) = do
	putStrLn ((show i) ++ s1 ++ name ++ s2 ++ number)
		where
			s1 = whitespace sp1
			s2 = whitespace sp2

buildEntry :: IO Entry
buildEntry = do
	name <- readName
	number <- readNumber
	return (name, number)

readName :: IO Name
readName = do
	putStrLn "Name: "
	name <- getLine
	return name

readNumber :: IO Number
readNumber = do
	putStrLn "Number: "
	number <- getLine
	return number

readChar :: IO Char
readChar = do
	c <- getLine
	return (head (safeify c))

safeify :: [Char] -> [Char]
safeify s
	| s == "" = " "
	| otherwise = s

spacetid1 :: DB -> Int
spacetid1 [] = 0
spacetid1 db = length(show (length db))

spacetid2 :: DB -> Int
spacetid2 [] = 0
spacetid2 db
	| (longestName db ) > 0 = (longestName db - 2)
	| otherwise = 0

spacetab1 :: DB -> Int -> Int
spacetab1 [] _ = 0
spacetab1 db i = (spacetid1 db) + 2 - length(show i)

spacetab2 :: DB -> String -> Int
spacetab2 [] _ = 0
spacetab2 db s
	| (longestName db - 2) > 0 = (longestName db) + 4 - length s
	| otherwise = 0

whitespace :: Int -> [Char]
whitespace 0 = []
whitespace i
	| i > 0     = ' ' : (whitespace (i-1))
	| otherwise = error "whitespace: Negative length"