module Database where

type Name = String
type Number = String
type Entry = (Name, Number)
type DB = [Entry]

getName :: Entry -> Name
getName (name, _)     = name

getNumber :: Entry -> Number
getNumber (_, number) = number

addtoDB :: Entry -> DB -> DB
addtoDB e es = es ++ [e]

getEntryByID :: Int -> DB -> Entry
getEntryByID 0 (x:xs) = x
getEntryByID _ []     = error "getEntryByID: DB too short"
getEntryByID i (x:xs)
	| i > 0     = getEntryByID (i-1) xs
	| otherwise = error "getEntryByID: Negative ID" 

editEntryByID :: Int -> Entry -> DB -> DB
editEntryByID 0 e (x:xs) = e : xs
editEntryByID _ _ []     = error "editEntryByID: DB too short"
editEntryByID i e (x:xs)
	| i > 0     = x : (editEntryByID (i-1) e xs)
	| otherwise = error "editEntryByID: Negative ID" 

removeEntryByID :: Int -> DB -> DB
removeEntryByID 0 (x:xs) = xs
removeEntryByID _ []     = error "removeEntryByID: DB too short"
removeEntryByID i (x:xs)
	| i > 0     = x : (removeEntryByID (i-1) xs)
	| otherwise = error "removeEntryByID: Negative ID"

longestName :: DB -> Int
longestName db = lname 0 db

lname :: Int -> DB -> Int
lname m []     = m
lname m (e:es)
	| (length (getName e)) > m = lname (length (getName e)) es
	| otherwise = lname m es

longestNumber :: DB -> Int
longestNumber db = lnumber 0 db

lnumber :: Int -> DB -> Int
lnumber m []     = m
lnumber m (e:es)
	| (length (getNumber e)) > m = lnumber (length (getNumber e)) es
	| otherwise = lnumber m es