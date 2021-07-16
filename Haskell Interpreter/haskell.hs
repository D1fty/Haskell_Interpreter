import System.IO
import Data.Char
import Data.String
import Data.Bool

-- Input Main and getchar loops 
main :: IO ()
main = do putStrLn "--------------------------------------------------------------"
          putStrLn "             159.341 Assignment 1 Semester 1 2021"
          putStrLn "           Submitted by: Damien DeCourcy, 19042551"
          putStrLn "--------------------------------------------------------------"
          collectOne [] [("TAB", ['\t']), ("SPACE", [' ']), ("NEWLINE", ['\n'])] 1

collectOne :: String -> [(String, String)] -> Int -> IO ()
collectOne str symbolsOne cmd = do done <- isEOF
                                   if done
                                   then return ()
                                   else do input <- getChar
                                           if input == ';'
                                           then do symbols <- action (validCommand (takeIdentifier (dropWhile isSpace str)) (dropWhile isSpace (dropIdentifier (dropWhile isSpace str))) symbolsOne) symbolsOne cmd
                                                   if head symbols == ("EXIT", "EXIT")
                                                   then return ()
                                                   else collectOne [] symbols (cmd + 1)
                                           else if input == '"'
                                                then do this <- collectTwo (str ++ [input])
                                                        if this == "~~EXIT~~"
                                                        then return ()
                                                        else collectOne this symbolsOne cmd
                                                else collectOne (str ++ [input]) symbolsOne cmd

collectTwo :: String -> IO String
collectTwo str = do done <- isEOF
                    if done
                    then return "~~EXIT~~"
                    else do input <- getChar
                            if input == '"'
                            then return (str ++ [input])
                            else collectTwo (str ++ [input])

-- Validation functions
commandValue :: String -> Int
commandValue string
 | string == "print"  || string == "printlength" || string == "printwords" || string == "printwordcount" = 3
 | string == "append" || string == "reverse"                                                             = 2
 | string == "set"                                                                                       = 1
 | string == "list" || string == "exit"                                                                  = 4
 | otherwise = 0

validCommand :: String -> String -> [(String, String)] -> (Bool, String)
validCommand command cmdstring symbols
 | value == 1                   = validateExpression command cmdstring symbols [] True  False True
 | value == 2                   = validateExpression command cmdstring symbols [] True  True  True
 | value == 3                   = validateExpression command cmdstring symbols [] False False True
 | value == 4 && null cmdstring = (True, command ++ [' '] ++ cmdstring)
 | value == 4                   = (False, "found input after cmd")
 | otherwise                    = (False, "Command not found")
 where value  = commandValue command

-- Pattern matching in this expression must be done in this specific order
validateExpression :: String -> String -> [(String, String)] -> String -> Bool -> Bool -> Bool -> (Bool, String)
validateExpression cmd [] _ otherstring _ _ _ = (True, cmd ++ [' '] ++ otherstring)
validateExpression cmd cmdstring symbols otherstring False False False
 | head cmdstring == '+'                                          = validateExpression cmd (dropWhile isSpace (tail cmdstring)) symbols (otherstring ++ ['+']) False False True
 | otherwise                                                      = (False, "multiple values in expression not seperated by '+' symbol")
validateExpression cmd cmdstring symbols otherstring True idHasValue True
 | not (isLetter (head cmdstring))                                = (False, "initial identifier must start with a letter")
 | cantReadIdentifier (takeIdentifier cmdstring)                  = (False, "initial identifier has invalid symbol")
 | idHasValue && not (inTable (takeIdentifier cmdstring) symbols) = (False, "initial identifier not found in symbol table")
 | otherwise                                                      = validateExpression cmd (dropWhile isSpace (dropIdentifier cmdstring)) symbols (otherstring ++ takeIdentifier cmdstring ++ [' ']) False False True
validateExpression cmd cmdstring symbols otherstring False _ True
 | (head cmdstring == '"') && validLiteral (tail cmdstring)       = validateExpression cmd (dropWhile isSpace (dropLiteral (tail cmdstring))) symbols (otherstring ++ ['"'] ++ getLiteral (tail cmdstring) ++ ['"']) False False False
 | head cmdstring == '"'                                          = (False, "unterminated literal in expression")
 | not (isLetter (head cmdstring))                                = (False, "expression identifier did not start with letter")
 | cantReadIdentifier (takeIdentifier cmdstring)                  = (False, "unexpected symbol in expression identifier")
 | not (inTable (takeIdentifier cmdstring) symbols)               = (False, "expression contains reference to identifier not found in table")
 | otherwise                                                      = validateExpression cmd (dropWhile isSpace (dropIdentifier cmdstring)) symbols (otherstring ++ takeIdentifier cmdstring ++ [' ']) False False False

-- Parsing functions
cantReadIdentifier :: String -> Bool
cantReadIdentifier [] = False
cantReadIdentifier (head : tail)
 | isLetter head || isNumber head = cantReadIdentifier tail
 | otherwise                      = True

isConstant :: String -> Bool
isConstant string
 | string == "SPACE" || string == "TAB" || string == "NEWLINE" = True
 | otherwise                                                   = False

takeIdentifier :: String -> String
takeIdentifier [] = []
takeIdentifier (head : tail)
 | isLetter head || isDigit head = head : takeIdentifier tail
 | otherwise                     = []

dropIdentifier :: String -> String
dropIdentifier [] = []
dropIdentifier (head : tail)
 | isLetter head || isDigit head = dropIdentifier tail
 | otherwise                     = head: tail

validLiteral :: String -> Bool
validLiteral []    = False
validLiteral [';'] = False
validLiteral (head : tail)
 | head == '"'     = True
 | otherwise       = validLiteral tail

getLiteral :: String -> String
getLiteral (head : tail)
 | head == '"' = []
 | otherwise   = head : getLiteral tail

dropLiteral :: String -> String
dropLiteral (head : tail)
 | head == '"' = tail
 | otherwise   = dropLiteral tail

-- From here down none of the errors should be caught as they are validated out
-- These errors are just for exhaustive pattern completions sake
getIdentifierValue :: String -> [(String, String)] -> String
getIdentifierValue [] _      = error "Empty identifier passed: But how did you get here?"
getIdentifierValue symbol [] = error symbol ++ " not found in symbol table: But this should have been validated out?"
getIdentifierValue symbol ((tSymbol, tValue) : tail)
 | symbol == tSymbol         = tValue
 | otherwise                 = getIdentifierValue symbol tail

inTable :: String -> [(String, String)] -> Bool
inTable _ []      = False
inTable symbol ((word, _) : tail)
 | symbol == word = True
 | otherwise      = inTable symbol tail

parseExpression :: String -> [(String, String)] -> String
parseExpression [] _ = []
parseExpression (head : tail) symbolList
 | isSpace head  = parseExpression tail symbolList
 | head == '+'   = parseExpression tail symbolList
 | head == '"'   = getLiteral tail ++ parseExpression (dropLiteral tail) symbolList
 | otherwise     = getIdentifierValue (takeIdentifier (head : tail)) symbolList ++ parseExpression (dropIdentifier (head : tail)) symbolList

-- IO Operations
action :: (Bool, String) -> [(String, String)] -> Int -> IO [(String, String)]
action (False, errorString) symbols cmd = do putStrLn ("ERROR: " ++ errorString ++ " - in command #" ++ show cmd)
                                             return symbols
action (True, strTwo) symbols cmd
 | takeIdentifier strTwo == "set"            = return (set     (takeIdentifier (dropWhile isSpace (dropIdentifier strTwo))) (parseExpression (dropWhile isSpace (dropIdentifier (dropWhile isSpace (dropIdentifier strTwo)))) symbols) symbols)
 | takeIdentifier strTwo == "append"         = return (append  (takeIdentifier (dropWhile isSpace (dropIdentifier strTwo))) (parseExpression (dropWhile isSpace (dropIdentifier (dropWhile isSpace (dropIdentifier strTwo)))) symbols) symbols)
 | takeIdentifier strTwo == "list"           = do putStr "Identifier List ("
                                                  putStr (show (length symbols - 3))
                                                  putStrLn ")"
                                                  list symbols
                                                  return symbols
 | takeIdentifier strTwo == "exit"           = return [("EXIT", "EXIT")]
 | takeIdentifier strTwo == "print"          = do putStrLn (parseExpression (dropWhile isSpace (dropIdentifier strTwo)) symbols)
                                                  return symbols
 | takeIdentifier strTwo == "printlength"    = do putStr "Length is: "
                                                  print   (length (parseExpression (dropWhile isSpace (dropIdentifier strTwo)) symbols))
                                                  return symbols
 | takeIdentifier strTwo == "printwords"     = do putStrLn "Words are: "
                                                  myPrint (words  (parseExpression (dropWhile isSpace (dropIdentifier strTwo)) symbols))
                                                  return symbols
 | takeIdentifier strTwo == "printwordcount" = do putStr "Wordcount is: "
                                                  print  (length  (words (parseExpression (dropWhile isSpace (dropIdentifier strTwo)) symbols)))
                                                  return symbols 
 | takeIdentifier strTwo == "reverse"        = return (reverseSymbolValue (takeIdentifier (dropWhile isSpace (dropIdentifier strTwo))) symbols)
 | otherwise                                 = do error "invalid command throw reach:: What did you do? How did you get here?"

set :: String -> String -> [(String, String)] -> [(String, String)]
set symbol value symbolTable
 | inTable symbol symbolTable = replace symbol value symbolTable
 | otherwise                  = symbolTable ++ [(symbol, value)]

replace :: String -> String -> [(String, String)] -> [(String, String)]
replace _ _ []       = error "reached empty symbol table in replace"
replace symbol value ((tSymbol, tValue) : tail) 
 | symbol == tSymbol = (symbol, value ) : tail
 | otherwise         = (tSymbol, tValue) : replace symbol value tail

append :: String -> String -> [(String, String)] -> [(String, String)]
append _ _ []        = error "reached empty symbol table in append"
append symbol value ((tSymbol, tValue) : tail) 
 | symbol == tSymbol = (symbol, tValue ++ value) : tail
 | otherwise         = (tSymbol, tValue) : append symbol value tail

list :: [(String, String)] -> IO ()
list [] = error "reached empty symbol table in list"
list [(symbol, value)]
 | isConstant symbol     = do putStr []
 | endsInLineBreak value = do putStr symbol
                              putStr ": "
                              putStr value
 | otherwise             = do putStr symbol
                              putStr ": "
                              putStrLn value
list ((symbol, value) : tail) 
 | isConstant symbol     = list tail
 | endsInLineBreak value = do putStr symbol
                              putStr ": "
                              putStr value
                              list tail
 | otherwise             = do putStr symbol
                              putStr ": "
                              putStrLn value
                              list tail

endsInLineBreak :: String -> Bool
endsInLineBreak []            = False
endsInLineBreak ['\n']        = True
endsInLineBreak (head : tail) = endsInLineBreak tail

reverseSymbolValue :: String -> [(String, String)] -> [(String, String)]
reverseSymbolValue _ [] = error "reached empty symbol table in reverseSymbolValue"
reverseSymbolValue symbol ((tSymbol, value) : tail)
 | symbol == tSymbol    = (tSymbol, myReverse (words value)) : tail
 | otherwise            = (tSymbol, value) : reverseSymbolValue symbol tail

myReverse :: [String] -> String
myReverse []               = []
myReverse (headString : tailString)
 | hasGrammar headString   = myReverse tailString ++ " " ++ reverseWithGrammar headString []
 | otherwise               = myReverse tailString ++ " " ++ headString

hasGrammar :: String -> Bool
hasGrammar []     = False
hasGrammar (head : tail)
 | isGrammar head = True
 | otherwise      = hasGrammar tail

isGrammar :: Char -> Bool
isGrammar head
 | head == '.'   = True
 | head == ','   = True
 | head == '!'   = True
 | head == '?'   = True
 | head == ':'   = True
 | head == ';'   = True
 | head == '"'   = True
 | head == '\''  = True
 | otherwise     = False

isInnerSymbol :: Char -> Bool
isInnerSymbol char
 | char == '\'' || char == '-' = True
 | otherwise                   = False

reverseWithGrammar :: String -> String -> String
reverseWithGrammar [] output = output
reverseWithGrammar (head : tail) output
 | isGrammar head = reverseWithGrammar tail [] ++ [head] ++ output
 | otherwise      = reverseWithGrammar tail (output ++ [head])

myPrint :: [String] -> IO ()
myPrint []       = putStr ""
myPrint (h : tail) 
 | length h > 1      = do myPrintWord h
                          myPrint tail
 | isLetter (head h) = do putStrLn h
                          myPrint tail
 | otherwise         = myPrint tail

myPrintWord :: String -> IO ()
myPrintWord []                         = putStr ['\n']
myPrintWord [char]
 | isLetter char                       = putStrLn [char]
 | otherwise                           = myPrintWord []                                           
myPrintWord (head : tail)
 | isLetter head || isInnerSymbol head = do putStr [head]
                                            myPrintWord tail
 | otherwise                           = myPrintWord tail