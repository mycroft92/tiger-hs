{
{-# LANGUAGE DeriveFoldable #-}
module Parser
   where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
import qualified AST as A
import AST((<<->>))
import qualified TokenTypes as T
import Errors 

}

%name parse top
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
  '.'        { L.RangedToken T.Dot _}
  ';'        { L.RangedToken T.Semi _}
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

%right '->'
%right ';'
%left function type var
%right else in do then while if
%nonassoc ':='
%left end
%nonassoc to of
%left '|' '&'
%left '+' '-'
%left '*' '/'
%left NEG
%nonassoc '=' '<>' '<' '>' '<=' '>='
%right '('
--%left ')'

%%

-- some helper rules



optional(p)
  :   { Nothing }
  | p { Just $1 }

tyfields :: {[A.RecField]}
  : identifier ':' identifier {[unTok $1 (\rng (T.Identifier n) -> unTok $3 (\rng2 (T.Identifier ty) -> A.RecField n False ty ($1 <-> $3)))]}
  | tyfields ',' identifier ':' identifier {unTok $3 (\rng (T.Identifier n) -> unTok $5 (\rng2 (T.Identifier ty) -> A.RecField n False ty ($3 <-> $5))) : $1}

ty
  : identifier          {unTok $1 (\rng (T.Identifier id) -> A.NameTy id  rng)}
  | '{' tyfields '}'    {A.RecordTy (reverse $2) ($1 <->$3)}
  | array of identifier {unTok $3 (\rng (T.Identifier id) -> A.ArrayTy id ($1 <->$3))}

tydec
  : type identifier '=' ty {unTok $2 (\rng (T.Identifier n) -> A.TypeD n $4 ($1 <->>$4))}
 
tydecs
  : tydec         { [$1]}
  | tydecs tydec  %shift{ $2:$1}

vardec :: {A.Dec}
  : var identifier ':=' exp {unTok $2 (\rng (T.Identifier n) -> A.VarDec n False Nothing $4 ($1 <->>$4))}
  | var identifier ':' identifier ':=' exp {unTok $2 (\rng (T.Identifier n) -> unTok $4 (\rng2 (T.Identifier ty) -> A.VarDec n False (Just ty) $6 ($1 <->>$6)))}

fundec
  : function identifier '(' tyfields ')' '=' exp {unTok $2 (\rng (T.Identifier n) -> A.FunDec n $4 Nothing $7 ($1 <->>$7))}
  | function identifier '(' ')' '=' exp {unTok $2 (\rng (T.Identifier n) -> A.FunDec n [] Nothing $6 ($1 <->>$6))}
  | function identifier '(' tyfields ')' ':' identifier '=' exp {unTok $2 (\rng (T.Identifier n) -> unTok $7 (\_ (T.Identifier rt) -> A.FunDec n $4 (Just rt) $9 ($1 <->>$9)))}
  | function identifier '(' ')' ':' identifier '=' exp {unTok $2 (\rng (T.Identifier n) -> unTok $6 (\_ (T.Identifier rt) -> A.FunDec n [] (Just rt) $8 ($1 <->>$8)))}

fundecs
  : fundec         %shift{[$1]}
  | fundecs fundec %shift{$2:$1}

dec :: {A.Dec}
  : tydecs  %shift{A.TypeDec $1 (listRange (reverse $1))}
  | fundecs %shift{A.FunctionDec $1 (listRange (reverse $1))}
  | vardec  {$1}

decs
  : dec {[$1]}
  | decs dec {$2:$1}

lval :: {A.Var}
  : identifier          %shift{unTok $1 (\rng (T.Identifier n)-> A.SimpleVar n rng) }
  | lval '.' identifier %shift{unTok $3 (\rng (T.Identifier n)-> A.FieldVar $1 n ($1 <<-> $3)) }
  | lval '[' exp ']'    %shift{A.SubscriptVar $1 $3 ($1 <<-> $4)}

commaExps :: {[A.Exp]}
    :        {[]}
    | exp    {[$1]}
    | commaExps ',' exp {$3:$1} -- needs to reversed at use site

seqExps_ :: {[A.Exp]}
    : exp ';'       {[unTok $2 (\rng _ -> A.NilExp rng),$1]}
    | exp ';' exp   {[$3,$1]}
    | seqExps_ ';' exp {$3:$1}

seqExps :: {A.Exp}
        : '(' seqExps_ ')'    {A.SeqExp (reverse $2) ($1 <-> $3)}

{-binop 
    : '*'  {A.Times}
    | '/'  {A.Divide}
    | '-'  {A.Minus}
    | '+'  {A.Plus}
    | '='  {A.Eq}
    | '<>' {A.Neq}
    | '<'  {A.Lt}
    | '>'  {A.Gt}
    | '<=' {A.Le}
    | '>=' {A.Ge}
    | '&'  {A.LAnd}
    | '|'  {A.LOr} -}


recordExp :: {[A.Field]}
    :           {[]}
    | identifier '=' exp { [unTok $1 (\rng (T.Identifier n) -> A.Field n  $3 ($1 <->> $3))] } 
    | recordExp ',' identifier '=' exp { unTok $3 (\rng (T.Identifier n) -> A.Field n  $5 ($3 <->> $5)): $1 } 

exp :: {A.Exp}
    : integer {unTok $1 (\rng (T.Integer int) -> A.IntExp int rng)}
    | string  {unTok $1 (\rng (T.String s) -> A.StringExp s rng)}
    | nil     {unTok $1 (\rng (T.Nil) -> A.NilExp rng)}
    | identifier '[' exp ']' of exp {unTok $1 (\rng (T.Identifier n) -> A.ArrayExp n $3 $6 ($1 <->> $6))} 
    | lval    {A.VarExp $1}
    | identifier '{' recordExp '}'  { unTok $1 (\rng (T.Identifier n) -> A.RecordExp n $3 ($1<->$4))}
    | identifier '(' commaExps ')' {unTok $1 (\rng (T.Identifier n) -> A.CallExp n (reverse $3) ($1 <-> $4))}
    | lval ':=' exp { A.AssignExp $1 $3 ($1 <<->> $3)}
    | seqExps       {$1}
    --| '(' seqExps ')'    {A.SeqExp (reverse $2) ($1 <-> $3)}
    | '-' exp %prec NEG  {A.UnopExp $2 ($1 <->>$2)}
    | while exp do exp {A.WhileExp $2 $4 ($1 <->>$4)}
    | break            {A.BreakExp (range $ L.rtRange $1)}
    | for identifier ':=' exp to exp do exp {unTok $2 (\rng (T.Identifier s) -> A.ForExp s False $4 $6 $8 ($1 <->> $8)) }
    | if exp then exp else exp  {A.IfExp $2 $4 (Just $6) ($1 <->> $6)}
    | if exp then exp     {A.IfExp $2 $4 Nothing ($1 <->>$4)}
    | let decs in exp end {A.LetExp (reverse $2) $4 ($1 <-> $5)}
    | let  in exp end {A.LetExp [] $3 ($1 <-> $4)}
    | let  in '(' ')' end {A.LetExp [] (A.NilExp ($3 <-> $4)) ($1 <-> $4)}
    | let decs in '(' ')' end {A.LetExp (reverse $2) (unTok $3 (\rng _ ->A.NilExp rng)) ($1 <-> $4)}
    | let decs in end {A.LetExp (reverse $2) (unTok $3 (\rng _ ->A.NilExp rng)) ($1 <-> $4)}
    | exp '*' exp  %shift{A.BinopExp $1 A.Times $3 ($1<<->>$3)}
    | exp '/' exp  %shift{A.BinopExp $1 A.Divide $3 ($1<<->>$3)}
    | exp '+' exp  %shift{A.BinopExp $1 A.Plus $3 ($1<<->>$3)}
    | exp '-' exp  %shift{A.BinopExp $1 A.Minus $3 ($1<<->>$3)}
    | exp '=' exp        {A.BinopExp $1 A.Eq $3 ($1<<->>$3)}
    | exp '<>' exp       {A.BinopExp $1 A.Neq $3 ($1<<->>$3)}
    | exp '<' exp        {A.BinopExp $1 A.Lt $3 ($1<<->>$3)}
    | exp '>' exp        {A.BinopExp $1 A.Gt $3 ($1<<->>$3)}
    | exp '<=' exp       {A.BinopExp $1 A.Le $3 ($1<<->>$3)}
    | exp '>=' exp       {A.BinopExp $1 A.Ge $3 ($1<<->>$3)}
    | exp '&' exp  %shift{A.BinopExp $1 A.LAnd $3 ($1<<->>$3)}
    | exp '|' exp  %shift{A.BinopExp $1 A.LOr $3 ($1<<->>$3)}
    | '(' exp ')' {$2}

top :: {A.Exp}
    : exp {$1}
    |     {A.NilExp (A.Range (A.Pos 0 0) (A.Pos 0 0))}
    

{

unTok :: L.RangedToken -> (A.Range -> T.Token -> a) -> a
unTok (L.RangedToken tok rng) ctor = ctor (range rng) tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<=>) :: L.Range -> L.Range -> A.Range
L.Range a1 _ <=> L.Range _ b2 = range (L.Range a1 b2)

(<->) :: L.RangedToken -> L.RangedToken -> A.Range
L.RangedToken _ (L.Range a1 _)  <-> L.RangedToken _ (L.Range _ b2)  = range (L.Range a1 b2)


(<<->) :: A.Rangers a => a -> L.RangedToken -> A.Range
a <<-> (L.RangedToken _ rng) = (A.getRange a) <<->> (range rng)

(<->>) :: A.Rangers a => L.RangedToken -> a -> A.Range
(L.RangedToken _ rng) <->> a = (range rng) <<->> (A.getRange a)

range :: L.Range -> A.Range
range r@(L.Range start stop) = A.Range {A.start = start', A.stop = stop'}
    where
        posc  (L.AlexPn a l c) = A.Pos l c
        start' = posc start
        stop'  = posc stop

listRange :: A.Rangers a => [a] -> A.Range
--listRange []  = A.Range (A.Pos -1 -1) (A.Pos -1 -1)
listRange [x] = A.getRange x
listRange (f:lst) = f <<->> (lst !! (length lst -1))

data ParserState = ParserState { errorList :: [Errors]}

parseError :: L.RangedToken -> L.Alex a
parseError (L.RangedToken rtoken rng) = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.putError (ParserError $ "Parse error at line " <> show rtoken <> " @" <> show line <> ", column " <> show column)
  L.alexError $ "Parse error at line "  <> show rtoken <> " @"<> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)


}
