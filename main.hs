import Control.Exception (evaluate)
import Data.List
import System.IO
import Text.Regex.PCRE

matchesHeader :: String -> Bool
matchesHeader a = a =~ "^//.*$|^$"

matchesImport :: String -> Bool
matchesImport a = a =~ "^(#|@)(import|include).*$|^$"

matchesBracket :: String -> Bool
matchesBracket a = a =~ "^.+<.+>$"

importSorter :: String -> String -> Ordering
importSorter ('@':_) ('#':_) = LT
importSorter ('#':_) ('@':_) = GT
importSorter ('#':xs) ('#':ys) = importCompare xs ys
importSorter [] _ = GT
importSorter _ [] = LT

importCompare :: String -> String -> Ordering
importCompare a b =
  case (matchA, matchB) of
    (True, False) -> LT
    (False, True) -> GT
    (_, _) -> compare a b
  where matchA = matchesBracket a
        matchB = matchesBracket b

sortImports :: String -> [String]
sortImports x = do
  let input = lines x
  let (before, rest) = span matchesHeader input
  let (imports, after) = span matchesImport rest
  let sorted = sortBy importSorter $ filter (not . null) imports
  before ++ sorted ++ []:after

main = do
  contents <- readFile "example.m"
  writeFile "example2.m" $ unlines $ sortImports contents
