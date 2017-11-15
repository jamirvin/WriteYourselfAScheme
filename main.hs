module Main where
import           LispEvaluator
import           LispParser
import           System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
