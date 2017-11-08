module Main where
import           Data.Char
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving (Show)

main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn $ readExpr expr

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseEscapeChar :: Parser Char
parseEscapeChar = do _ <- char '\\'
                     c <- oneOf "\\\"nrt"
                     return $ case c of
                                'n' -> '\n'
                                'r' -> '\r'
                                't' -> '\t'
                                _   -> c

parseNonEscape :: Parser Char
parseNonEscape = noneOf "\\\"\n\r\t"

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (parseNonEscape <|> parseEscapeChar)
                 _ <- char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = fmap Number $ number <|> radixHex <|> radixDec <|> radixBin <|> radixOct
                where number = read <$> many1 digit
                      radixDec = try $ string "#d" >> number
                      radixHex = try $ fmap (fst . head . readHex) (string "#x" >> many1 hexDigit)
                      radixBin = try $ fmap readBin (string "#b" >> many1 (oneOf "01"))
                      readBin = fst . head . readInt 2 (`elem` "01") digitToInt
                      radixOct = try $ fmap (fst . head . readOct) (string "#o" >> many1 octDigit)

parseExpr :: Parser LispVal
parseExpr = parseNumber
            <|> parseString
            <|> parseAtom
