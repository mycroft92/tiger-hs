{
module Lexer (
    Alex,
    AlexPosn (..),
    alexGetInput,
    alexError,
    runAlex,
    alexMonadScan,

    Range (..),
    RangedToken(..),
    scanMany
) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Conditional (when)
import TokenTypes (Token(..))

}

-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]


@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

<0> $white+ ;

<0>       "(*" { nestComment `andBegin` comment }
<0>       "*)" { \_ _ -> alexError "Error: unexpected closing comment" }
<0>       \"   {andBegin initString string}
<0> "//"[^\n]* {skip}
<comment> "(*" { nestComment }
<comment> "*)" { unnestComment }
<comment> .    ;
<comment> \n   ;
-- TODO
<string> \\\\  {handleEscape '\\'}
<string> \\\"  {handleEscape '"'}
<string> $white+ {addString}
<string> \\n   {handleEscape '\n'}
<string> \\v   {handleEscape '\v'}
<string> \\t   {handleEscape '\t'}
<string> \"    {endParseString}
<string> .     {parseString}




-- Keywords
<0> let      {tok Let}
<0> in       {tok In}
<0> if       {tok If}
<0> then     {tok Then}
<0> else     {tok Else}
<0> type     {tok DType}
<0> while    {tok While}
<0> for      {tok For}
<0> end      {tok End}
<0> array    {tok Array}
<0> do       {tok Do}
<0> nil      {tok Nil}
<0> function {tok Function}
<0> break    {tok Break}
<0> of       {tok Of}
<0> to       {tok To}
<0> var      {tok Var}

-- Arithmetic operators
<0> "+"     {tok Plus}
<0> "-"     {tok Minus}
<0> "*"     {tok Times}
<0> "/"     {tok Divide}

-- Comparison operators
<0> "="     {tok Eq}
<0> "<>"    {tok Neq}
<0> "<"     {tok Lt}
<0> "<="    {tok Le}
<0> ">"     {tok Gt}
<0> ">="    {tok Ge}

-- Logical operators
<0> "&"     {tok And}
<0> "|"     {tok Or}

-- Parenthesis
<0> "("     {tok LPar}
<0> ")"     {tok RPar}
<0> "{"     {tok LBrace}
<0> "}"     {tok RBrace}

-- Lists
<0> "["     {tok LBrack}
<0> "]"     {tok RBrack}
<0> ","     {tok Comma}

-- Types
<0> ":"     {tok Colon}
<0> "->"    {tok Arrow}

-- Type operator
<0> "'"     {tok Quote}

-- Identifiers
<0> @id     {tokId}

-- Constants
<0> $digit+ {tokInteger}

{
-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
  {
    nestLevel :: Int,
    currentString :: [Char],
    stringStart :: AlexPosn
  }

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
  nestLevel = 0,
  currentString = "",
  stringStart = AlexPn 0 0 0
}

-- Monad state manipulators
get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)


put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())


alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment at end of file"
  when (startCode == string) $
    alexError "Error: unclosed string literal at end of file"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)


-- AlexInput has type: (AlexPosn,    -- current position,
--                  Char,        -- previous char
--                  ByteString,  -- current input string
--                  Int64)       -- bytes consumed so far
-- type AlexAction result = AlexInput -> Int64 -> Alex result
mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier  $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }


scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Integer $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

nestComment, unnestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

initString, parseString, addString, endParseString :: AlexAction RangedToken
initString inp@(start,_,_,_) len = do
  modify $ \s -> s{stringStart = start, currentString = "\""}
  skip inp len
parseString inp@(_,_,str,_) len = do
  st <- get
  modify $ \s -> s{currentString = (currentString st) ++ (BS.unpack $ BS.take len str)}
  skip inp len
endParseString inp@(end,_,_,_) len = do
  state <- get
  let out = makeString (currentString state ++ "\"") (stringStart state) end
  put state{currentString = "",stringStart = AlexPn 0 0 0}
  alexSetStartCode 0
  skip inp len
  return out
  where
    makeString :: [Char] -> AlexPosn -> AlexPosn -> RangedToken
    makeString str start end = RangedToken { rtToken = String str
      , rtRange = Range{start = start, stop = end} 
      }

addString inp@(_, _, str, _) len = do
  st <- get
  modify $ \s -> s{currentString = (currentString st) ++ (BS.unpack $ BS.take len str)}
  skip inp len

handleEscape :: Char -> AlexAction RangedToken
handleEscape c inp len = do
  state <- get
  let st = (currentString state) ++[c]
  put state{currentString = st}
  skip inp len
}
