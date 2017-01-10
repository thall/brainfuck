module Interpreter
( Env(..)
, newEnv
, eval
) where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Array
import Data.Word
import Parser
import Data.Functor.Identity

data Env = Env
  { tape :: Array Int Word8
  , dataPtr :: Int
  } deriving (Show)

newEnv :: Env
newEnv = Env (array (1,500) [(i,0) | i <- [1..500]]) 1

eval :: [Brainfuck] -> [Word8] -> WriterT [Word8] (State Env) ()
eval []  _                                = return ()
eval (IncrPntr:xs) input                  = do
  modify $ movePtr (+1)
  eval xs input

eval (DecrPntr:xs) input                  = do
  modify $ movePtr (subtract 1)
  eval xs input

eval (Incr:xs) input                      = do
  modify $ modifyValue (+1)
  eval xs input

eval (Decr:xs) input                      = do
  modify $ modifyValue (subtract 1)
  eval xs input

eval (OutputAtPtr:xs) input               = do
  (Env t d) <- get
  tell $ (t ! d) : []
  eval xs input

eval (AcceptByteAtPtr:xs) []  = do
  error "lol"
eval (AcceptByteAtPtr:xs) (input:inputs)  = do
  (Env t d) <- get
  let newEnv2 =  Env (t // [(d, input)]) d
  put $ newEnv2
  eval xs inputs

eval rs@((Loop p):xs) input               = do
  env@(Env t d) <- get
  if (isZero env) then do eval xs input    
  else do eval p input 
          eval rs input

isZero :: Env -> Bool
isZero (Env t ptr) = t ! ptr == 0

movePtr :: (Int -> Int) -> Env -> Env
movePtr f (Env t ptr) = Env t (f ptr)

modifyValue :: (Word8 -> Word8) -> Env -> Env
modifyValue f (Env t d) = Env (bumpArray t d f) d

bumpArray :: Array Int Word8 -> Int -> (Word8 -> Word8) -> Array Int Word8
bumpArray arr idx f =
  let newValue = f (arr ! idx)
  in arr // [(idx, newValue)]   


-- Tests

