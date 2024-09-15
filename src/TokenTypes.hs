module TokenTypes (
    Token (..)
)where
import AST (Exp(ForExp, AssignExp), Var)
import Data.Array (Array)
import GHC.RTS.Flags (DoCostCentres)
import Data.Sequence.Internal.Sorting (QList(Nil))
import Text.Megaparsec (ErrorItem(EndOfInput))

data Token
  -- Identifiers
  = Identifier String
  -- Constants
  | String String
  | Integer Int
  -- Keywords
  | Let
  | In
  | If
  | Then
  | Else
  | DType
  | For
  | While
  | Array
  | Function
  | Var
  | Do
  | Nil
  | End
  | Break
  | Of
  | To
  -- Arithmetic operators
  | Plus
  | Minus
  | Times
  | Divide
  | Dot
  -- Comparison operators
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  -- Logical operators
  | And
  | Or
  -- Assign
  | Assign
  -- Parenthesis
  | LPar
  | RPar
  | LBrace
  | RBrace
  -- Lists
  | Comma
  | LBrack
  | RBrack
  -- Types
  | Colon
  | Arrow
  | Quote
  -- EOF
  | EOF
  deriving (Eq, Show)
