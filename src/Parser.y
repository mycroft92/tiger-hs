{
{-# LANGUAGE DeriveFoldable #-}
module Parser
  ( parse
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
import qualified AST as A
import qualified TokenTypes as T
import Errors 

}

%name parse exp
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken T.EOF _ }
%token
  -- Identifiers
  identifier { L.RangedToken (T.Identifier _) _ }
  -- Constants
  string     { L.RangedToken (T.String _) _ }
  integer    { L.RangedToken (T.Integer _) _ }
  -- Keywords
  let        { L.RangedToken T.Let _ }
  in         { L.RangedToken T.In _ }
  if         { L.RangedToken T.If _ }
  then       { L.RangedToken T.Then _ }
  else       { L.RangedToken T.Else _ }
  type       { L.RangedToken T.DType _ }
  for        { L.RangedToken T.For _ }
  while      { L.RangedToken T.While _ }
  array      { L.RangedToken T.Array _ }
  function   { L.RangedToken T.Function _ }
  var        { L.RangedToken T.Var _ }
  do         { L.RangedToken T.Do _ }
  nil        { L.RangedToken T.Nil _ }
  end        { L.RangedToken T.End _ }
  break      { L.RangedToken T.Break _ }
  of         { L.RangedToken T.Of _ }
  to         { L.RangedToken T.To _ }
  -- Arithmetic operators
  '+'        { L.RangedToken T.Plus _ }
  '-'        { L.RangedToken T.Minus _ }
  '*'        { L.RangedToken T.Times _ }
  '/'        { L.RangedToken T.Divide _ }
  ':='       { L.RangedToken T.Assign _}
  -- Comparison operators
  '='        { L.RangedToken T.Eq _ }
  '<>'       { L.RangedToken T.Neq _ }
  '<'        { L.RangedToken T.Lt _ }
  '<='       { L.RangedToken T.Le _ }
  '>'        { L.RangedToken T.Gt _ }
  '>='       { L.RangedToken T.Ge _ }
  -- Logical operators
  '&'        { L.RangedToken T.And _ }
  '|'        { L.RangedToken T.Or _ }
  -- Parenthesis
  '('        { L.RangedToken T.LPar _ }
  ')'        { L.RangedToken T.RPar _ }
  '{'        { L.RangedToken T.LBrace _} 
  '}'        { L.RangedToken T.RBrace _} 
  -- Lists
  '['        { L.RangedToken T.LBrack _ }
  ']'        { L.RangedToken T.RBrack _ }
  ','        { L.RangedToken T.Comma _ }
  -- Types
  ':'        { L.RangedToken T.Colon _ }
  '->'       { L.RangedToken T.Arrow _ }

%%

-- some helper rules
many_rev(p)
  :               { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

some_rev(p)
  : p             {[$1]}
  | some_rev(p) p {$2:$1}
 
some(p)
  : some_rev(p)   {reverse $1}

optional(p)
  :   { Nothing }
  | p { Just $1 }

tyfields
  :                           {}
  | identifier ':' identifier {}
  | tyfields ',' identifier ':' identifier {}

ty
  : identifier          {}
  | '{' tyfields '}'    {}
  | array of identifier {}

tydec
  : type identifier '=' ty {}
 
tydecs
  : some(tydec)  { $1}

vardec
  : var identifier ':=' exp {}
  | var identifier ':' identifier ':=' exp {}

dec
  : tydecs  {}
  | vardec  {}
  | fundecs {}

decs
  : many(dec) {$1}

exp :: {A.Exp}
    : integer { unTok $1 (\rng (T.Integer int) -> A.IntExp int (range rng))}



{

unTok :: L.RangedToken -> (L.Range -> T.Token -> a) -> a
unTok (L.RangedToken tok rng) ctor = ctor rng tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> A.Range
L.Range a1 _ <-> L.Range _ b2 = range (L.Range a1 b2)


range :: L.Range -> A.Range
range r@(L.Range start stop) = A.Range {A.start = start', A.stop = stop'}
    where
        posc  (L.AlexPn a l c) = A.Pos l c
        start' = posc start
        stop'  = posc stop

data ParserState = ParserState { errorList :: [Errors]}

parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}
