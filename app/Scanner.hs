module Scanner
( Program
, Unchecked
, Checked
, fromCheckedProgram
, toProgram
, scan
) where

data Program a = Program String
  deriving (Show)

data Unchecked
data Checked

fromCheckedProgram :: Program Checked -> String
fromCheckedProgram (Program p) = p

toProgram :: String -> Program Unchecked
toProgram = Program

scan :: Program Unchecked -> Either String (Program Checked)
scan (Program p) = syntaxCheck $ scan_  p

scan_ :: String -> String
scan_ []       = []
scan_ ('>':xs) = '>' : scan_ xs
scan_ ('<':xs) = '<' : scan_ xs
scan_ ('+':xs) = '+' : scan_ xs
scan_ ('-':xs) = '-' : scan_ xs
scan_ ('.':xs) = '.' : scan_ xs
scan_ (',':xs) = ',' : scan_ xs
scan_ ('[':xs) = '[' : scan_ xs
scan_ (']':xs) = ']' : scan_ xs
scan_ (_:xs)   = scan_ xs

syntaxCheck :: String -> Either String (Program Checked)
syntaxCheck p = case syntaxCheck_ p 0 
                of (Just 0) -> Right $ Program p
                   (Just _) -> Left "Uneven parentheses (Too many opening ones)"
                   Nothing  -> Left "Uneven parentheses (Too many closing ones)"
	where
	syntaxCheck_ :: String -> Int -> Maybe Int
	syntaxCheck_ [] i            = Just i
	syntaxCheck_ ('[' : xs) i    = syntaxCheck_ xs (i+1)
	syntaxCheck_ (']' : xs) 0    = Nothing
	syntaxCheck_ (']' : xs) i    = syntaxCheck_ xs (i-1)
	syntaxCheck_ (_ : xs) i      = syntaxCheck_ xs i
