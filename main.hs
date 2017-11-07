module Main where
import           Control.Monad
import           System.Environment
import           System.IO
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

parseEscapeChar :: Parser String
parseEscapeChar = do _ <- char '\\'
                     c <- oneOf "\\\"0nrvtbf"
                     return [c]

parseNonEscape :: Parser String
parseNonEscape = return <$> noneOf "\\\""

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (parseNonEscape <|> parseEscapeChar)
                 _ <- char '"'
                 return $ String $ concat x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
