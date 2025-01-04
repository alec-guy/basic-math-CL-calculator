{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where 

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import System.IO 
-- Parser
import Text.Megaparsec
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer 
import Text.Megaparsec.Error 
import Control.Monad.Combinators 
import Control.Monad.Combinators.Expr 
import Control.Applicative as A (empty)
import Data.Void (Void)
import Data.List as L (singleton)
import Control.Monad
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Data.List.NonEmpty as NE (fromList, head)
import Data.Set as Set 

showLastExpression :: StateT (Maybe (MathExpr Double)) IO ()
showLastExpression = do 
    expr' <- get 
    case expr' of 
        Nothing -> return () 
        (Just expr'') -> lift $ do 
                    putStrLn $ "Expression = " ++ (show $ expr'')
                    putStrLn $ "Evaluation = " ++ (show $ eval expr'')

getCalculatorInput :: StateT (Maybe (MathExpr Double)) IO () 
getCalculatorInput = do 
    lift $ do 
        putStr ":"
        hFlush stdout 
    input <- lift getLine 
    case parse parseCalculatorInput ""  input of 
        Left err -> lift $ putStrLn $ errorBundlePretty err 
        Right calcCommandOrExpr -> 
            case calcCommandOrExpr of 
                Left command -> 
                    case command of 
                        Exit -> lift $ do 
                            putStrLn "Thank you for using this calculator"
                            exitSuccess 
                        Help -> do 
                            lift $ showHelpMenu 
                            getCalculatorInput
                Right exp    -> put (Just exp) 
                    

runCalculator :: StateT (Maybe (MathExpr Double)) IO () 
runCalculator = do 
    getCalculatorInput 
    showLastExpression 
    runCalculator

showHelpMenu :: IO ()
showHelpMenu = do 
    putStrLn "! to quit"
    putStrLn "help for this menu"
    putStrLn "Normal mode is to enter mathematical expressions"

main :: IO () 
main = do 
    putStrLn "=Calculator="
    evalStateT runCalculator Nothing

------------------------------------------------------------------
-- Types
data MathExpr a where 
    Add     ::  MathExpr Double -> MathExpr Double -> MathExpr Double
    Sub     ::  MathExpr Double -> MathExpr Double -> MathExpr Double  
    Mult    ::  MathExpr Double -> MathExpr Double -> MathExpr Double 
    Div     ::  MathExpr Double -> MathExpr Double -> MathExpr Double
    Pow     ::  MathExpr Double -> MathExpr Double -> MathExpr Double 
    Neg     ::  MathExpr Double -> MathExpr Double 
    Number  ::  Double -> MathExpr Double 
    Trig    :: MathExpr Double -> MathExpr Double

instance Show (MathExpr Double) where 
    show (Number d)      = show d 
    show (Neg expr)      = "- " ++ (show expr)
    show (Add expr expr') =  (parenthesize expr) ++ " + " ++ (parenthesize expr')
    show (Sub expr expr')  =  (parenthesize expr) ++ " - " ++ (parenthesize expr')
    show (Mult expr expr') = (parenthesize expr) ++ " * " ++ (parenthesize expr')
    show (Div expr expr')  = (parenthesize expr) ++ " / " ++ (parenthesize expr')
    show (Pow expr expr') =  (parenthesize expr) ++ " ^ " ++ (parenthesize expr')

parenthesize :: MathExpr Double -> String 
parenthesize expr =  "(" ++ (show expr) ++ ")"

data Command where 
    Exit :: Command 
    Help :: Command
    deriving (Show)
----------------------------------------------------------------------
-- Evaluator 

eval :: MathExpr Double -> Double 
eval mathexpr = 
    case mathexpr of 
     (Number n) -> n  
     (Neg expr) -> negate (eval expr)
     (Add expr expr2) -> (eval expr) + (eval expr2)
     (Sub expr expr2) -> (eval expr) - (eval expr2)
     (Mult expr expr2) -> (eval expr) * (eval expr2)
     (Div expr expr2) -> (eval expr) / (eval expr2)
     (Pow expr expr2) -> (eval expr) ^ (round (eval expr2))
-----------------------------------------------------------------
-- Parser 
type Parser = Parsec Void String 
spaceParser :: Parser () 
spaceParser = 
    Text.Megaparsec.Char.Lexer.space hspace1 A.empty A.empty 
lexemeParser :: Parser a -> Parser a 
lexemeParser = lexeme spaceParser 

parseNumber ::  Parser (MathExpr Double) 
parseNumber = lexemeParser $ do 
    num <- many digitChar
    case (readMaybe num) :: Maybe Double of 
        Nothing  -> 
            case Prelude.null num of 
                True  -> fancyFailure (Set.singleton $ ErrorFail "empty input")
                False -> unexpected (Label (NE.fromList $ num))
        (Just x) -> return $ Number x

parseCalculatorInput :: Parser (Either Command (MathExpr Double))
parseCalculatorInput = eitherP parseCommand expr 

parseExit :: Parser Command
parseExit = do 
    void $ string "!"
    return $ Exit 

help :: Parser Command 
help = Help <$ string "help"

parseCommand :: Parser Command 
parseCommand = 
    choice [parseExit, help]



expr = lexemeParser (makeExprParser term table <?> "expression")
term = lexemeParser ((between  (string "(") (string ")")) expr <|> parseNumber)
    

table = 
    [[Prefix (Neg <$ lexemeParser (string "-"))
     ,Prefix (Trig <$ lexemeParser parseTrig)
     ]
    ,[InfixL (Pow <$ lexemeParser (string "^"))
     ,InfixL (Mult <$ lexemeParser (string "*"))
     ,InfixL (Div <$ lexemeParser (string "/"))
     ]
    ,[InfixL (Add <$ lexemeParser (string "+"))
     ,InfixL (Sub <$ lexemeParser (string "-"))
     ]
    ]
parseTrig :: Parser (MathExpr Double)
parseTrig = do 
    choice [string "sin", string "cos", string "tan"]
----------------------------------------------------------------
