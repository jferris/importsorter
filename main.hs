import Control.Monad ((<=<))
import Data.List (sort)
import Text.Parsec
import Text.Parsec.String

data ImportType = Package | System | Local deriving (Eq, Ord)
data Import = Import ImportType String deriving (Eq, Ord)
type Header = String
type Body = String
data File = File Header [Import] Body

main = do
    result <- parseFromFile fileParser "example.m"
    case result of
        Left error -> print error
        Right file -> writeFile "example2.m" $ sortImports file

fileParser :: Parser File
fileParser = do
    comments <- many commentParser
    imports <- many importParser
    body <- bodyParser
    return $ File (concat comments) imports body

commentParser :: Parser Header
commentParser = concatParser [string "//", tillEol, many1 newline]

importParser :: Parser Import
importParser = do
    result <-
        try packageImportParser <|>
        try systemImportParser <|>
        try localImportParser
    many1 newline
    return result

packageImportParser :: Parser Import
packageImportParser =
    return . Import Package =<< concatParser [string "@", tillEol]

systemImportParser :: Parser Import
systemImportParser = return . Import System =<< hashImportParser "<"

localImportParser :: Parser Import
localImportParser = return . Import Local =<< hashImportParser "\""

hashImportParser :: String -> Parser String
hashImportParser bracket =
    concatParser
        [ string "#"
        , string "import" <|> string "include"
        , many1 $ char ' '
        , string bracket
        , tillEol
        ]

bodyParser :: Parser Body
bodyParser = manyTill anyChar eof

tillEol :: Parser String
tillEol = many $ noneOf "\n"

concatParser :: [Parser String] -> Parser String
concatParser = return . concat <=< sequence

sortImports :: File -> String
sortImports (File header imports body) =
    concat $ [header, sortedImports, "\n",  body]
  where sortedImports = unlines $ map importToString $ sort imports

importToString :: Import -> String
importToString (Import _ string) = string
