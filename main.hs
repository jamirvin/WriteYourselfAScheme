module Main where
import           Data.Array
import           Data.Char
import           Data.Complex
import           Data.Ratio
import qualified Data.Vector                   as V
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Rational Rational
             | Complex (Complex Double)
             | String String
             | Character Char
             | Vector (V.Vector LispVal)
             | Bool Bool deriving (Show)

main :: IO ()
main = do (expr:_) <- getArgs
          putStrLn $ readExpr expr

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right v  -> "Found value: " ++ show v

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

parseRational :: Parser LispVal
parseRational = try $ do n <- many1 digit
                         _ <- char '/'
                         m <- many1 digit
                         return $ Rational $ read n % read m

toDouble :: LispVal -> Double
toDouble (Float n) = realToFrac n
toDouble (Number n) = fromInteger n
toDouble (Complex n) = realPart n
toDouble (Rational n) = realToFrac n
toDouble x = error $ "LispVal " ++ show x ++ " cannot be converted to Double"

parseComplex :: Parser LispVal
parseComplex = try $ do n <- try parseFloat <|> parseNumber
                        _ <- char '+'
                        m <- try parseFloat <|> parseNumber
                        _ <- char 'i'
                        return $ Complex (toDouble n :+ toDouble m)

parseChar :: Parser LispVal
parseChar = try $ fmap Character $ string "#\\" >> (charName <|> charLiteral)
            where
              charName = try (spaceName <|> newLine)
              spaceName = string "space" >> return ' '
              newLine = string "newline" >> return '\n'
              charLiteral = do c <- anyChar
                               notFollowedBy alphaNum
                               return c

parseNumberTypes :: Parser LispVal
parseNumberTypes = parseComplex <|> parseRational <|> parseFloat <|> parseNumber

parseExpr :: Parser LispVal
parseExpr = parseNumberTypes
            <|> parseChar
            <|> parseString
            <|> parseVector
            <|> parseAtom
            <|> parseSugarTypes
            <|> do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do h <- endBy parseExpr spaces
                     t <- char '.' >> spaces >> parseExpr
                     return $ DottedList h t

parseSugarTypes :: Parser LispVal
parseSugarTypes = parseQuoted <|> parseQuasiquote <|> parseUnquote

parseSugar :: Char -> String -> Parser LispVal
parseSugar sugar expansion = do _ <- char sugar
                                x <- parseExpr
                                return $ List [Atom expansion, x]

parseQuoted :: Parser LispVal
parseQuoted = parseSugar '\'' "quote"

parseQuasiquote :: Parser LispVal
parseQuasiquote = parseSugar '`' "quasiquote"

parseUnquote :: Parser LispVal
parseUnquote = parseSugar ',' "unquote"

parseVector :: Parser LispVal
parseVector = try $ do _ <- string "#("
                       l <- sepBy parseExpr spaces
                       _ <- char ')'
                       return . Vector . V.fromList $ l
