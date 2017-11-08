module Main where
import           Data.Char
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Character Char
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
                      radixHex = try $ fmap (val readHex) (string "#x" >> many1 hexDigit)
                      radixBin = try $ fmap readBin (string "#b" >> many1 (oneOf "01"))
                      readBin = val $ readInt 2 (`elem` "01") digitToInt
                      radixOct = try $ fmap (val readOct) (string "#o" >> many1 octDigit)

val :: ReadS a -> String -> a
val r = fst . head . r

parseFloat :: Parser LispVal
parseFloat = fmap (Float . val readFloat) floatString
               where
                 floatString = try $ do n <- many1 digit
                                        decimal <- char '.'
                                        m <- many1 digit
                                        return $ n ++ [decimal] ++ m

parseChar :: Parser LispVal
parseChar = try $ fmap Character $ string "#\\" >> (charName <|> charLiteral)
            where
              charName = try (spaceName <|> newLine)
              spaceName = string "space" >> return ' '
              newLine = string "newline" >> return '\n'
              charLiteral = do c <- anyChar
                               notFollowedBy alphaNum
                               return c

parseExpr :: Parser LispVal
parseExpr = parseFloat
            <|> parseNumber
            <|> parseChar
            <|> parseString
            <|> parseAtom
