module LispParser
(
  LispNumber,
  LispVal(Atom, List, DottedList, Number, String, Character, Vector, Bool),
  readExpr
) where
import           Data.Char
import           Data.Complex
import           Data.Ratio
import qualified Data.Vector                   as V
import           Numeric
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispNumber = Integer Integer
             | Float Double
             | Rational Rational
             | Complex (Complex Double) deriving (Show)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNumber
             | String String
             | Character Char
             | Vector (V.Vector LispVal)
             | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom a) = a
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Vector v) = "#(" ++ (unwordsList . V.toList) v ++ ")"
showVal (Character c) = show c

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right v  -> v

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

parseInteger :: Parser LispNumber
parseInteger = fmap Integer $ number <|> radixHex <|> radixDec <|> radixBin <|> radixOct
                where number = read <$> many1 digit
                      radixDec = try $ string "#d" >> number
                      radixHex = try $ fmap (val readHex) (string "#x" >> many1 hexDigit)
                      radixBin = try $ fmap readBin (string "#b" >> many1 (oneOf "01"))
                      readBin = val $ readInt 2 (`elem` "01") digitToInt
                      radixOct = try $ fmap (val readOct) (string "#o" >> many1 octDigit)

val :: ReadS a -> String -> a
val r = fst . head . r

parseFloat :: Parser LispNumber
parseFloat = fmap (Float . val readFloat) floatString
               where
                 floatString = try $ do n <- many1 digit
                                        decimal <- char '.'
                                        m <- many1 digit
                                        return $ n ++ [decimal] ++ m

parseRational :: Parser LispNumber
parseRational = try $ do n <- many1 digit
                         _ <- char '/'
                         m <- many1 digit
                         return $ Rational $ read n % read m

toDouble :: LispNumber -> Double
toDouble (Float n) = realToFrac n
toDouble (Integer n) = fromInteger n
toDouble (Complex n) = realPart n
toDouble (Rational n) = realToFrac n

parseComplex :: Parser LispNumber
parseComplex = try $ do n <- try parseFloat <|> parseInteger
                        _ <- char '+'
                        m <- try parseFloat <|> parseInteger
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

parseNumber :: Parser LispVal
parseNumber = Number <$> (parseComplex <|> parseRational <|> parseFloat <|> parseInteger)

parseExpr :: Parser LispVal
parseExpr = parseNumber
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
