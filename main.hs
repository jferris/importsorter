import Control.Applicative (Applicative, (<$>), (<*>), (<*))
import Control.Monad ((<=<))
import Data.List (sort)
import Data.Monoid (Monoid, mappend)
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
fileParser = File <$> headerParser <*> many importParser <*> bodyParser

headerParser :: Parser Header
headerParser = concat <$> many commentParser

commentParser :: Parser Header
commentParser = string "//" <++> tillEol <++> many1 newline

importParser :: Parser Import
importParser = parser <* many1 newline
  where
    parser =
        try packageImportParser <|>
        try systemImportParser <|>
        try localImportParser

packageImportParser :: Parser Import
packageImportParser = Import Package <$> string "@" <++> tillEol

systemImportParser :: Parser Import
systemImportParser =  Import System <$> hashImportParser "<"

localImportParser :: Parser Import
localImportParser = Import Local <$> hashImportParser "\""

hashImportParser :: String -> Parser String
hashImportParser bracket =
    string "#" <++>
    (string "import" <|> string "include") <++>
    (many1 $ char ' ') <++>
    string bracket <++>
    tillEol

bodyParser :: Parser Body
bodyParser = manyTill anyChar eof

tillEol :: Parser String
tillEol = many $ noneOf "\n"

(<++>) :: (Applicative f, Monoid m) => f m -> f m -> f m
a <++> b = mappend <$> a <*> b

sortImports :: File -> String
sortImports (File header imports body) =
    concat $ [header, sortedImports, "\n",  body]
  where sortedImports = unlines $ map importToString $ sort imports

importToString :: Import -> String
importToString (Import _ string) = string
