module UserInterface where

import Database

welcome :: DB -> IO ()
welcome db = do
	putStrLn "\nWelcome to the Phonebook interactive user interface.\n"
	interaction db

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
	putStrLn ""
	i <- getidio db
	putStrLn "\nIs\n"
	printEntry i 2 2 (getEntryByID i db)
	putStrLn "\nthe Entry yo want to edit? (y|n)"
	a <- readChar
	edio db a i

removeio :: DB -> IO ()
removeio db = do
	putStrLn ""
	i <- getidio db
	putStrLn "\nDo you really want to remove\n"
	printEntry i 2 2 (getEntryByID i db)
	putStrLn "\n? (y|n)"
	a <- readChar
	putStrLn ""
	remio db a i

printio :: DB -> IO ()
printio db = do
	if (length db) == 0
		then do
			putStrLn "\nYour DB is empty.\n"
			interaction db
		else do

	putStrLn ("\n ID" ++ (whitespace (spacetid1 db)) ++ "Name" ++ (whitespace (spacetid2 db))++ "Number\n")
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
	putStrLn "  h(elp)    Print this help\n"
	interaction db

messio :: DB -> IO ()
messio db = do
	putStrLn "\nYou clearly don't know how to use the interface. I strongly encourage you to use the h(elp).\n"
	interaction db

getidio :: DB -> IO (Int)
getidio db = do
	putStrLn "For this action we need the entry's ID. Do you know it? (y|n)"
	i <- readChar
	putStrLn ""
	if i == 'y'
		then do
			it <- getterio db
			return (it)
		else do
			putStrLn ("\n ID" ++ (whitespace (spacetid1 db)) ++ "Name" ++ (whitespace (spacetid2 db))++ "Number\n")
			printEntries  db db 0
			putStrLn ""
			it <- getterio db
			return (it)
			

getterio :: DB -> IO (Int)
getterio db = do
	putStrLn "What is the ID of the Entry you want to work with?"
	i <- readInt
	return i

remio :: DB -> Char -> Int -> IO ()
remio db c i
	| c == 'y' = do
		interaction (removeEntryByID i db)
	|otherwise = do
		interaction db

--edio :: DB -> Char -> Int -> IO ()
--edio db c i
--	| c == 'y' = do
--		putStrLn "\nPlease specify the entry you want to add:"
--		e <- buildEntry
--		putStrLn ""
--		interaction (editEntryByID i e db)
--	|otherwise = do
--		interaction db

edio :: DB -> Char -> Int -> IO ()
edio db c i
	| c == 'y' = do
		putStrLn "\nDou you want to change the name, the number or both? (n|u|b|e)"
		ch <- readChar
		putStrLn ""
		edmad db ch i
	|otherwise = do
		interaction db

edmad :: DB -> Char -> Int -> IO ()
edmad db c i
	| c == 'n'  = do
		name <- readName
		putStrLn ""
		interaction (editEntryByID i (name, (getNumber(getEntryByID i db))) db)
	| c == 'u'  = do
		number <- readNumber
		putStrLn ""
		interaction (editEntryByID i ((getName(getEntryByID i db)), number) db)
	| c == 'b'  = do
		name <- readName
		number <- readNumber
		putStrLn ""
		interaction (editEntryByID i (name, number) db)
	| c == 'e'  = interaction db
	| otherwise = edio db 'y' i

printEntries :: DB -> DB -> Int -> IO ()
printEntries _ [] _ = return ()
printEntries db (e:es) i
	| i >= 0     = do
		printEntry i (spacetab1 db i) (spacetab2 db (getName e)) e
		printEntries db es (i+1)
	| otherwise = error "editEntryByID: Negative ID"

printEntry :: Int -> Int -> Int -> Entry -> IO ()
printEntry i sp1 sp2 (name, number) = do
	putStrLn (" " ++ (show i) ++ s1 ++ name ++ s2 ++ number)
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

readInt :: IO Int
readInt = do
	i <- getLine
	return (read i)

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
	| (longestName db - 2) > 0 = (longestName db) + 2 - length s
	| otherwise = 0

whitespace :: Int -> [Char]
whitespace 0 = []
whitespace i
	| i > 0     = ' ' : (whitespace (i-1))
	| otherwise = error "whitespace: Negative length"
