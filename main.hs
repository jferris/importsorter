import Control.Exception (evaluate)
import Data.List
import System.IO
import Text.Regex.PCRE

bracketRegex :: String
bracketRegex = "^.+<.+>$"

matchesBracket :: String -> Bool
matchesBracket a = a =~ bracketRegex

importSorter :: String -> String -> Ordering
importSorter ('@':_) ('#':_) = LT
importSorter ('#':_) ('@':_) = GT
importSorter ('#':xs) ('#':ys) = importCompare xs ys
importSorter [] _ = GT
importSorter _ [] = LT

importCompare :: String -> String -> Ordering
importCompare a b =
  case (matchA, matchB) of
    (True, True) -> compare a b
    (True, False) -> LT
    (False, True) -> GT
    (False, False) -> compare a b
  where matchA = matchesBracket a
        matchB = matchesBracket b

matchesJunk :: String -> Bool
matchesJunk a = a =~ "^//.*$|^$"

matchesGood :: String -> Bool
matchesGood a = a =~ "^(#|@)(import|include).*$|^$"

sortImports :: String -> [String]
sortImports x = do
  let input = lines x
  let (before, rest) = span matchesJunk input
  let (imports, after) = span matchesGood rest
  let sorted = sortBy importSorter  $ filter (not . null) imports
  before ++ sorted ++ []:after

main = do
  handle <- openFile "example.m" ReadMode
  contents <- hGetContents handle
  evaluate (length contents)
  handle <- openFile "example2.m" WriteMode
  mapM_ (hPutStrLn handle) $ sortImports contents
  hClose handle
