module Main where

import Scanner
import Parser
import Interpreter
import Data.Word
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  brainfuckProgram <- Prelude.readFile "app/HelloWorld.b"
  expectedOutput <- Prelude.readFile "app/Counter.out"
  
  Prelude.putStrLn brainfuckProgram
  Prelude.putStrLn expectedOutput
  
  Prelude.putStrLn $ toString $ runProgram ( parseProgram $ checkProgram brainfuckProgram ) []
  


run :: String -> [Word8] -> (((), [Word8]), Env)
run p i = runState (runWriterT $ eval ( parseProgram $ checkProgram p ) i) newEnv

seven = "++>+++++[<+>-]"

hello = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]"
   
loop = ">++[>,<-]>."

toString :: [Word8] -> String
toString s  = C.unpack $ pack s

checkProgram :: String -> Program Checked
checkProgram str = case scan (toProgram str) of 
                  (Left msg) -> error msg
                  (Right p) -> p

parseProgram :: Program Checked -> [Brainfuck]
parseProgram = parse


runProgram :: [Brainfuck] -> [Word8] -> [Word8]
runProgram p i = snd $ evalState (runWriterT $ eval p i) newEnv

--
-- import Control.Monad.State
-- import Data.Array
-- import Data.Maybe
-- import Data.Char
-- import Data.Either
-- 
-- data Brainfuck
--   = IncrPntr
--   | DecrPntr
--   | Incr
--   | Decr
--   | OutputAtPtr
--   | AcceptByteAtPtr
--   | Loop [Brainfuck]
--   deriving (Show)
-- 
-- 
-- data Env = Env
--   { dataPtr :: Int
--   , tape :: Array Int Int
--   } deriving (Show)
-- 
-- doit :: String -> IO Env
-- doit p = evalP (fromJust $ parse $ fromRight $ syntaxCheck p) initState
-- 
-- lal :: String -> IO ((), Env)
-- lal p = evalA (fromJust $ parse p) initState
-- 
-- parse :: String -> Maybe [Brainfuck]
-- parse = sequence . parseStr
-- 
-- parseStr :: String -> [Maybe Brainfuck]
-- parseStr []       = []
-- parseStr ('[':xs) = let (p, ps) = getLoop xs
-- 	            in (Just $ Loop (fromJust $ sequence $ parseStr p)) : parseStr ps
-- parseStr (']':xs) = parseStr xs
-- parseStr (x:xs)   = parseChar x : parseStr xs
-- 
-- parseChar :: Char -> Maybe Brainfuck
-- parseChar '>' = Just IncrPntr
-- parseChar '<' = Just DecrPntr
-- parseChar '+' = Just Incr
-- parseChar '-' = Just Decr
-- parseChar '.' = Just OutputAtPtr
-- parseChar ',' = Just AcceptByteAtPtr
-- parseChar _   = Nothing
-- 
-- initState :: Env
-- initState = Env 1 (array (1,5) [(i,0) | i <- [1..5]])
-- 
-- evalP :: [Brainfuck] -> Env -> IO Env
-- evalP program e = execStateT (eval program) e
-- 
-- evalA :: [Brainfuck] -> Env -> IO ((), Env)
-- evalA program e = runStateT (eval program) e
-- 
-- eval :: [Brainfuck] -> StateT Env IO ()
-- eval []                   = return ()
-- eval (IncrPntr:xs)        = do
-- --  liftIO $ putStrLn $ "IncrPntr"
--   e <- get
--   put $ incPtr e
--   eval xs
-- eval (DecrPntr:xs)        = do
--  -- liftIO $ putStrLn $ "DecrPntr"
--   e <- get
--   put $ decrPtr e
--   eval xs
-- eval (Incr:xs)            = do
--   --liftIO $ putStrLn $ "Incr"
--   e <- get
--   put $ incr e
--   eval xs
-- eval (Decr:xs)            = do
--   -- liftIO $ putStrLn $ "Decr"
--   e <- get
--   put $ decr e
--   eval xs
-- eval (OutputAtPtr:xs)     = do
--   -- liftIO $ putStrLn $ "OutputAtPtr"
--   (Env d t) <- get
--   liftIO $ putStrLn $ show $ chr $ t ! d
--   eval xs
-- eval (AcceptByteAtPtr:xs) = do
--   -- liftIO $ putStrLn $ "AcceptByteAtPtr"
--   (Env d t) <- get
--   input <- liftIO $ getChar
--   put $ Env d (t // [(d, ord input)])
--   eval xs
-- eval rs@((Loop p):xs)          = do
--   -- liftIO $ putStrLn $ "Loop"
--   (Env d t) <- get
--   -- liftIO $ putStrLn $ "d = " ++ show d
--   -- liftIO $ putStrLn $ "Value t ! d = " ++ show(t ! d)
--   if (isZero (Env d t)) then do eval xs     
--   else do eval p 
-- 	  eval rs
-- 
-- isZero :: Env -> Bool
-- isZero (Env d t) = t ! d == 0
-- 
-- incPtr :: Env -> Env
-- incPtr (Env d t) = Env (d+1) t
-- 
-- decrPtr :: Env -> Env
-- decrPtr (Env d t) = Env (d-1) t
-- 
-- incr :: Env -> Env
-- incr (Env d t) = Env d (bumpArray t d (+1))
-- 
-- decr :: Env -> Env
-- decr (Env d t) = Env d (bumpArray t d (+(-1)))
-- 
-- bumpArray :: Array Int Int -> Int -> (Int -> Int) -> Array Int Int
-- bumpArray arr idx f =
--   let newValue = f (arr ! idx)
--   in arr // [(idx, newValue)]
-- 
-- 
-- 
-- 
-- p :: Brainfuck -> Int
-- p (Loop p)    = 1
-- p _         = 0
-- 
-- -- dataPtrIsThree :: Bool
-- -- dataPtrIsThree = (dataPtr $ evalP (parse ">>") env) == 3
-- 
-- syntaxCheck :: String -> Either String String
-- syntaxCheck p = case syntaxCheck_ p 0 
--  		of (Just n) -> if n == 0 then Right p else Left "Uneven parentheses"
--  		   Nothing  -> Left "Uneven parentheses"
-- 	where
-- 	syntaxCheck_ :: String -> Int -> Maybe Int
-- 	syntaxCheck_ [] i            = Just i
-- 	syntaxCheck_ ('(' : xs) i    = syntaxCheck_ xs (i+1)
-- 	syntaxCheck_ (')' : xs) i 
-- 		| i <= 0             = Nothing
-- 		| otherwise          = syntaxCheck_ xs (i-1)
-- 	syntaxCheck_ (_ : xs) i      = syntaxCheck_ xs i
-- 
-- fromRight :: Either String String -> String
-- fromRight (Left l)  = error l
-- fromRight (Right b) = b
-- 
-- getLoop :: String -> (String, String)
-- getLoop str = let (i, xs) = span (\x -> x /= ']') str
-- 	      in (i, if length xs == 0 then [] else tail xs)
-- 
-- test :: String
-- test = ",[>+<-]>."
-- 
-- printA :: String
-- printA = "++++++[>++++++++++<-]>+++++."
-- 
-- loop :: String
-- loop = ",>,[<.>-]"
-- 
-- loop2 :: String
-- loop2 = ",[>+<-]>."
-- 
-- test2 :: String
-- test2 = "+>>+++++++++++++++++++++++++++++<<[>>>[>>]<[[>>+<<-]>>-[<<]]>+<<[-<<]<]>+>>[-<<]<+++++++++[>++++++++>+<<-]>-.----.>."
-- 
-- multiply :: String
-- multiply = ",>,<[>[>+>+<<-]>>[-<<+>>]<<<-]>>"
-- 
-- main :: IO ()
-- main = do
--   putStrLn "hej"
