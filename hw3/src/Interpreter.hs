{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Interpreter where
-- :set -XOverloadedStrings
import           Control.Monad              (replicateM_, void)
import           Control.Monad.Catch        (Exception, MonadCatch, MonadThrow, catch, throwM)
import           Control.Monad.Cont         (callCC)
import           Control.Monad.Reader       (MonadReader, asks, local, runReaderT)
import           Control.Monad.State        (MonadIO, MonadState, StateT, get,
                                             liftIO, modify, put, runStateT)
import           Data.Functor.Identity
import qualified Data.Map.Strict            as Map
import           Data.Typeable              (Typeable)
import           Data.Void                  (Void)
import           System.IO
import           System.IO.Unsafe

import qualified Data.ByteString            as PackedStr
import qualified Data.ByteString.Internal   as BS (c2w)
import qualified Data.ByteString.UTF8       as S8
-- import Data.ByteString.Char8
-- import  Prelude (elem, Show, IO, String, Int, empty, show)

import           Text.Megaparsec
import           Text.Megaparsec.Byte       (alphaNumChar, char, eol, letterChar, string)
import qualified Text.Megaparsec.Byte.Lexer as L
import           Text.Megaparsec.Expr


data Expression = Const Int
    | Var String
    | BoolVal Bool
    | Add {a :: Expression, b :: Expression}
    | Sub {a :: Expression, b :: Expression}
    | Mul {a :: Expression, b :: Expression}
    | Div {a :: Expression, b :: Expression}
    | Eq {a :: Expression, b :: Expression}
    | NEq {a :: Expression, b :: Expression}
    | Lt {a :: Expression, b :: Expression}
    | Gt {a :: Expression, b :: Expression}
    | LtEq {a :: Expression, b :: Expression}
    | GtEq {a :: Expression, b :: Expression}
    | Let String Expression Expression deriving (Show, Read, Eq)
--     | Let Expr Var

data ExpressionException = DivisionByZero | UndefinedVariable String deriving (Show, Read, Eq, Typeable)
instance Exception ExpressionException

type ExpressionMap = Map.Map String Int

evalBool :: (MonadReader ExpressionMap t, MonadThrow t) => Expression -> t Bool
evalBool expr = case expr of
    (BoolVal e1_ ) -> return e1_
    (Eq a_ b_) -> do
        l_ <- eval a_
        r_ <- eval b_
        return $ l_ == r_
    (NEq a_ b_) -> do
        l_ <- eval a_
        r_ <- eval b_
        return $ l_ /= r_
    (Lt a_ b_) -> do
        l_ <- eval a_
        r_ <- eval b_
        return $ l_ < r_
    (Gt a_ b_) -> do
        l_ <- eval a_
        r_ <- eval b_
        return $ l_ > r_
    (GtEq a_ b_) -> do
        l_ <- eval a_
        r_ <- eval b_
        return $ l_ >= r_
    (LtEq a_ b_) -> do
        l_ <- eval a_
        r_ <- eval b_
        return $ l_ <= r_

eval :: (MonadReader ExpressionMap t, MonadThrow t) => Expression -> t Int
eval expr = case expr of
    (Const c) -> return c
    (Var x) -> asks (Map.lookup x) >>= maybe (throwM (UndefinedVariable x)) return
    (Add x y) ->  do
        a1 <- eval x
        b1 <- eval y
        return $ a1 + b1
    (Sub x y) -> do
        a1 <- eval x
        b1 <- eval y
        return $ a1 - b1
    (Mul x y) -> do
        a1 <- eval x
        b1 <- eval y
        return $ a1 * b1
    (Div x y) -> do
        a1 <- eval x
        b1 <- eval y
        case b1 of
            0 -> throwM DivisionByZero
            _ -> return $ a1 `div` b1
    (Let name value expr_) ->
        eval value >>= \val_ -> local (Map.insert name val_) (eval expr_)



runEval :: (MonadThrow t) => Expression -> ExpressionMap -> t Int
runEval expr = runReaderT (eval expr)

runEvalBool :: (MonadThrow t) => Expression -> ExpressionMap -> t Bool
runEvalBool expr = runReaderT (evalBool expr)

data Statement =
    Assignment  {var :: String, val :: Expression}
    | Reassignment  {var :: String, val :: Expression}
    | InState  {var :: String}
    | OutState  {val :: Expression}
    | IfState {cond :: Expression, e1 :: [Statement], e2 :: [Statement]}
    | Break {var :: String}
    | ForLoop  {val    :: Expression, to_ :: Expression, body  :: [Statement]}
    deriving (Read, Show, Eq, Typeable)


data StatementException =
    ValReassignmentException String
    | ComputationException Statement ExpressionException
    | UndefinedVariableException String
    deriving ( Show, Eq, Typeable)


instance Exception StatementException
type MutableContext = StateT ExpressionMap (Either StatementException)

assign :: (MonadState ExpressionMap t, MonadCatch t) => String -> Int -> t ()
assign variable value = do
    map_ <- get
    case Map.lookup variable map_ of
        Nothing -> put $ Map.insert variable value map_
        Just _  -> throwM (ValReassignmentException variable)

reassign :: (MonadState ExpressionMap t, MonadCatch t) => String -> Int -> t ()
reassign variable value = do
    map_ <- get
    case Map.lookup variable map_ of
        Just _  -> put $ Map.insert variable value map_
        Nothing -> throwM (UndefinedVariableException variable)

overwrite :: (MonadState ExpressionMap t, MonadCatch t) => String -> Int -> t ()
overwrite variable value = modify $ Map.insert variable value

evaluateExpression :: MonadCatch t => Statement -> Expression -> ExpressionMap -> t Int
evaluateExpression stmt expr varz = catch (runEval expr varz) (\e -> throwM $ ComputationException stmt e)

evaluateExpressionBool :: MonadCatch t => Statement -> Expression -> ExpressionMap -> t Bool
evaluateExpressionBool stmt expr varz = catch (runEvalBool expr varz) (\e -> throwM $ ComputationException stmt e)

evaluateStatement :: (MonadState ExpressionMap t, MonadCatch t, MonadIO t) => [Statement] -> t ExpressionMap
evaluateStatement statements = {-callCC $ \exit -> do-}
    case statements of
        [] -> get
        (x:xs) -> do
            let t = Assignment {var = "x", val = Const 1}
            vars <- get
            value <-
                case x of
                    InState _  -> read <$> liftIO getLine
                    IfState {} -> evaluateExpression t (val t) vars
                    _          -> evaluateExpression x (val x) vars
            cond <- case x of
                    IfState c _ _ -> evaluateExpressionBool x c vars
                    _             -> return True
            case x of
--                 (Break _) ->  exit "m"
                (IfState _ l r)  ->
                        replicateM_ 1 (if cond then evaluateStatement l else evaluateStatement r)
                (Assignment v _)   -> assign v value
                (Reassignment v _) -> reassign v value
                (InState v)        -> overwrite v value
                (OutState _)       -> liftIO $ print value
                (ForLoop _ to_ stmts) -> do
                            to <- evaluateExpression x to_ vars
                            replicateM_ (to - value) (evaluateStatement stmts)
            evaluateStatement xs


newtype StatementContext t = StatementContext {runStmt :: StateT ExpressionMap IO t}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState ExpressionMap, MonadThrow, MonadCatch)

executeProgram :: StatementContext t -> IO t
executeProgram ctx = fst <$> runStateT (runStmt ctx) Map.empty
runStatement :: [Statement] -> IO ExpressionMap
runStatement = executeProgram . evaluateStatement
runStatement_ :: [Statement] -> IO ()
runStatement_ = void . executeProgram . evaluateStatement


type Parser = Parsec Void S8.ByteString

newtype ParseException = ParseException (ParseError (Token S8.ByteString) Void)
    deriving (Read, Show, Eq, Typeable)

instance Exception ParseException

space :: Parser ()
space = skipSome (char (BS.c2w ' '))

indent :: Parser ()
indent = skipMany (char (BS.c2w ' '))

spaceConsumer :: Parser ()
spaceConsumer = L.space space empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: S8.ByteString -> Parser S8.ByteString
symbol = L.symbol spaceConsumer

lParenthesis :: Parser S8.ByteString
lParenthesis = symbol "("
rParenthesis :: Parser S8.ByteString
rParenthesis = symbol ")"
parenthesis :: Parser a -> Parser a
parenthesis = between lParenthesis rParenthesis
parenthesis_ :: Parser a -> Parser a
parenthesis_ = between (symbol "{") (symbol "}")

eqParser :: Parser S8.ByteString
eqParser = symbol "="

intParser :: Parser Int
intParser = lexeme L.decimal

skipWord :: S8.ByteString -> Parser ()
skipWord w = lexeme (string w *> notFollowedBy alphaNumChar)


pack :: ParsecT Void S8.ByteString Data.Functor.Identity.Identity S8.ByteString
pack = PackedStr.pack <$> ((:) <$> letterChar <*> many alphaNumChar)

keywordsList :: [S8.ByteString]
keywordsList = ["let", "mut", "for", "break", "true", "false", "in", "if", "then", "else"]

checkKeyWord :: Monad t => S8.ByteString -> t S8.ByteString
checkKeyWord w = if w `elem` keywordsList
                then fail $ "KeyWord !! - " ++ show w
                else return w

identifierParser :: Parser S8.ByteString
identifierParser = (lexeme . try) $
    do
    w <- pack
    checkKeyWord w

boolList :: [S8.ByteString]
boolList = ["true", "false"]

boolParser :: Parser Bool
boolParser = (lexeme . try) $ do
                            c <- pack
                            if c `elem` boolList
                                then return $ c == "true"
                                else fail $ "should be bool" ++ show c

identifierToStrParserSafe :: ParsecT Void S8.ByteString Data.Functor.Identity.Identity String
identifierToStrParserSafe = S8.toString <$> identifierParser

operators :: [[Operator Parser Expression]]
operators =
        [   [ InfixL (Eq <$ symbol "==")
            , InfixL (NEq <$ symbol "!=")
            , InfixL (GtEq <$ symbol ">=")
            , InfixL (LtEq <$ symbol "<=")
            , InfixL (Gt <$ symbol ">")
            , InfixL (Lt <$ symbol "<")
            ]
        ,
            [ InfixL (Mul <$ symbol "*")
            , InfixL (Div <$ symbol "/")
            ]
        ,   [ InfixL (Add <$ symbol "+")
            , InfixL (Sub <$ symbol "-")
            ]
        ]

termParser :: Parser Expression
termParser =  indent *> (parenthesis expressionParser
    <|> Const <$> intParser
    <|> Var <$> identifierToStrParserSafe
    <|> BoolVal <$> boolParser
    <|> Let
            <$> (skipWord "let" *> identifierToStrParserSafe <* eqParser)
            <*> (expressionParser <* symbol "in")
            <*> expressionParser)

expressionParser :: Parser Expression
expressionParser = makeExprParser termParser operators



statementParser :: Parser Statement
statementParser = indent *> (
          Assignment
                  <$> (skipWord "mut" *> identifierToStrParserSafe <* eqParser)
                  <*> expressionParser
          <|> OutState <$> (symbol "<" *> expressionParser)
          <|> InState <$> (symbol ">" *> identifierToStrParserSafe)
          <|> Break <$> (S8.toString <$> symbol "break")
          <|> ForLoop
                  <$> (skipWord "for" *> expressionParser)
                  <*> (skipWord "to" *> expressionParser )
                  <*> parenthesis_ programParser
          <|> IfState
                  <$> (skipWord "if" *> expressionParser)
                  <*> (skipWord "then" *> parenthesis_ programParser)
                  <*> (skipWord "else" *> parenthesis_ programParser)
          <|> Reassignment
                  <$> (identifierToStrParserSafe <* eqParser)
                  <*> expressionParser
        ) <* indent


parseExpressions :: MonadThrow t => S8.ByteString -> t Expression
parseExpressions  inp = either (throwM . ParseException) return $ parse expressionParser "" inp
parseWithExpressionEvaluation :: MonadThrow t => S8.ByteString -> t Int
parseWithExpressionEvaluation input = parseExpressions input >>= flip runEval Map.empty
parseStatement :: MonadThrow t => S8.ByteString -> t Statement
parseStatement inp =  either (throwM . ParseException) return $ parse statementParser "" inp
parseWithStatementEvaluation :: (MonadIO t, MonadCatch t) => S8.ByteString -> t ExpressionMap
parseWithStatementEvaluation input = parseStatement input >>= \stmt -> fst <$> runStateT (evaluateStatement [stmt]) Map.empty
programParser :: Parser [Statement]
programParser = spaceConsumer *> many (statementParser <* eol)
runProgram_ :: String -> S8.ByteString -> IO ExpressionMap
runProgram_ name input = either (throwM . ParseException) return (parse programParser name input) >>= runStatement


--
runProgram name file = do
    text <- PackedStr.readFile $ file
    runProgram_ name text
