import Control.Exception (evaluate)
import Data.List
import System.IO
import Text.Regex.PCRE

data ImportType = Package | System | Local deriving (Eq, Ord)
data Import = Import ImportType String deriving (Eq, Ord)

matchesHeader :: String -> Bool
matchesHeader a = a =~ "^//.*$|^$"

matchesImport :: String -> Bool
matchesImport a = a =~ "^(#|@)(import|include).*$|^$"

fromString :: String -> Import
fromString string
    | head string == '@' = Import Package string
    | matchesBracket string = Import System string
    | otherwise = Import Local string

importToString :: Import -> String
importToString (Import _ string) = string

matchesBracket :: String -> Bool
matchesBracket a = a =~ "^.+<.+>$"

sortImports :: String -> [String]
sortImports x = do
  let input = lines x
  let (before, rest) = span matchesHeader input
  let (imports, after) = span matchesImport rest
  let sorted = map importToString $ sort $ map fromString $ filter (not . null) imports
  before ++ sorted ++ []:after

main = do
  contents <- readFile "example.m"
  writeFile "example2.m" $ unlines $ sortImports contents
