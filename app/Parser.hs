module Parser
( Brainfuck(..)
, parse
) where

import Scanner

data Brainfuck
  = IncrPntr
  | DecrPntr
  | Incr
  | Decr
  | OutputAtPtr
  | AcceptByteAtPtr
  | Loop [Brainfuck]
  deriving (Eq)
  

instance Show Brainfuck where
  show (IncrPntr)         = ">"
  show (DecrPntr)         = "<"
  show (Incr)             = "+"
  show (Decr)             = "-"
  show (OutputAtPtr)      = "."
  show (AcceptByteAtPtr)  = ","
  show (Loop bs)          = showList bs ""

parse :: Program Checked -> [Brainfuck]
parse = parseStr . fromCheckedProgram

parseStr :: String -> [Brainfuck]
parseStr []       = []
parseStr ('[':xs) = let (p, ps) = getLoop xs
                    in (Loop $ parseStr p) : parseStr ps
parseStr (']':xs) = parseStr xs
parseStr (x:xs)   = parseChar x : parseStr xs

parseChar :: Char -> Brainfuck
parseChar '>' = IncrPntr
parseChar '<' = DecrPntr
parseChar '+' = Incr
parseChar '-' = Decr
parseChar '.' = OutputAtPtr
parseChar ',' = AcceptByteAtPtr

getLoop :: String -> (String, String)
getLoop str = let (i, xs) = span (\x -> x /= ']') str
              in (i, if length xs == 0 then [] else tail xs)


parseStr2 :: String -> [Brainfuck]
parseStr2 []       = []
parseStr2 ('[':xs) = [Loop $ parseStr2 xs]
parseStr2 (']':xs) = parseStr2 xs 
parseStr2 (x:xs)   = parseChar x : parseStr2 xs

