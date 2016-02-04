module Main where

import Control.Monad.State
import Data.Array
import Data.Maybe
import Data.Char

data Brainfuck
  = IncrPntr
  | DecrPntr
  | Incr
  | Decr
  | OutputAtPtr
  | AcceptByteAtPtr
  | IfZero
  | IfNonZero
  deriving (Show)


data Env = Env
  { dataPtr :: Int
  , tape :: Array Int Int
  } deriving (Show)

doit :: String -> IO Env
doit p = evalP (check $ fromJust $ parse p) initState

parse :: String -> Maybe [Brainfuck]
parse = sequence . parseStr

parseStr :: String -> [Maybe Brainfuck]
parseStr []     = []
parseStr (x:xs) = parseChar x : parseStr xs

parseChar :: Char -> Maybe Brainfuck
parseChar '>' = Just IncrPntr
parseChar '<' = Just DecrPntr
parseChar '+' = Just Incr
parseChar '-' = Just Decr
parseChar '.' = Just OutputAtPtr
parseChar ',' = Just AcceptByteAtPtr
parseChar '[' = Just IfZero
parseChar ']' = Just IfNonZero
parseChar _   = Nothing

evalP :: Maybe [Brainfuck] -> Env -> IO Env
evalP (Just program) e = execStateT (eval program) e
evalP _ _ = undefined

initState :: Env
initState = Env 1 (array (1,5) [(i,0) | i <- [1..5]])

evalA :: Maybe ([Brainfuck]) -> Env -> IO ((), Env)
evalA (Just program) e = runStateT (eval program) e
evalA _ _ = undefined


eval :: [Brainfuck] -> StateT Env IO ()
eval []                   = return ()
eval (IncrPntr:xs)        = do
  e <- get
  put $ incPtr e
  eval xs
eval (DecrPntr:xs)        = do
  e <- get
  put $ decrPtr e
  eval xs
eval (Incr:xs)            = do
  e <- get
  put $ incr e
  eval xs
eval (Decr:xs)            = do
  e <- get
  put $ decr e
  eval xs
eval (OutputAtPtr:xs)     = do
  (Env d t) <- get
  liftIO $ putChar $ chr $ t ! d
  eval xs
eval (AcceptByteAtPtr:xs) = do
  (Env d t) <- get
  input <- liftIO $ getChar
  put $ Env d (t // [(d, ord input)])
  eval xs
eval (IfZero:xs)          = return ()
eval (IfNonZero:xs)       = return ()

incPtr :: Env -> Env
incPtr (Env d t) = Env (d+1) t

decrPtr :: Env -> Env
decrPtr (Env d t) = Env (d-1) t

incr :: Env -> Env
incr (Env d t) = Env d (bumpArray t d (+1))

decr :: Env -> Env
decr (Env d t) = Env d (bumpArray t d (+(-1)))

bumpArray :: Array Int Int -> Int -> (Int -> Int) -> Array Int Int
bumpArray arr idx f =
  let newValue = f (arr ! idx)
  in arr // [(idx, newValue)]


check :: [Brainfuck] -> Maybe [Brainfuck]
check ps =
  let balanced = sum $ map p ps
  in if balanced == 0 then Just ps else Nothing

p :: Brainfuck -> Int
p IfZero    = 1
p IfNonZero = -1
p _         = 0

-- dataPtrIsThree :: Bool
-- dataPtrIsThree = (dataPtr $ evalP (parse ">>") env) == 3




main :: IO ()
main = do
  putStrLn "hej"
