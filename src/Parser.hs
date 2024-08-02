{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Parser where
    import Errors (Errors (..))

    -- import Text.Megaparsec.Char
    -- import Control.Monad.State (State,
    --   MonadIO(liftIO),
    --   MonadState(get,put),
    --   MonadTrans(lift),
    --   runState)
    import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))
    import qualified Data.Text as T
    import qualified Text.Megaparsec.Char.Lexer as L
    import Text.Megaparsec.Char ( char, char', space, space1 )
    import Text.Megaparsec hiding (State)
    import Text.Megaparsec.Error (errorBundlePretty)
    import AST
    import Data.Void (Void)
    import Data.Char (isAlphaNum, isAlpha, isPrint, toUpper, chr, isDigit)
    import Data.Text (unpack)
    -- import Control.Monad (void)
    import Data.Functor ( void, ($>) )


    -- data ParserState = ParserState {
    --     errors   :: [Errors],
    --     filename :: String
    -- }


    -- type Parser a = ParsecT Void T.Text (State ParserState) a
    type Parser = Parsec Void T.Text

    -- parse :: FilePath -> T.Text -> Either [Errors] Exp
    -- parse fname src = case runState (runParserT strparse fname src) (ParserState [] fname) of
    --   (Left err, st) -> Left $ ParserError (Text.Megaparsec.Error.errorBundlePretty err) : errors st
    --   (Right e, _)  -> Right e
    parse :: FilePath -> T.Text -> Either Errors Exp
    parse fname src = case runParser strparse fname src of
      Left err -> Left $ ParserError (Text.Megaparsec.Error.errorBundlePretty err)
      Right e  -> Right e

    getSrcPos :: Parser AST.Pos
    getSrcPos = do
      SourcePos _ linePos colPos <- getSourcePos
      let line = fromIntegral $ unPos linePos
      let col  = fromIntegral $ unPos colPos
      return (AST.Pos line col)

    keywords :: [String]
    keywords = ["for", "to", "while", "break", "let", "in", "if", "then", "else", "type", "end", "array", "function", "nil", "do", "var", "of"]

    spaces :: Parser ()
    spaces = L.space space1 (L.skipLineComment  "//") (L.skipBlockCommentNested "(*" "*)")

    symbol :: T.Text -> Parser T.Text
    symbol = L.symbol space

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme space

    brackets :: Parser a -> Parser a
    brackets = between (symbol "[") (symbol "]")

    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")

    integer :: Parser Int
    integer = lexeme L.decimal <?> "integer"


    annotate :: Parser (Range -> a) -> Parser a
    annotate m = do
      start <- getSrcPos
      f <- m
      f . Range start <$> getSrcPos

    strparse :: Parser Exp
    strparse = annotate (StringExp <$> strparser) <?> "string"
      where
        strparser :: Parser String
        strparser = do
          res <- between (char '"') (lexeme $ char '\"') (getstring "")
          return ('"': T.unpack res ++ "\"")

        getstring :: T.Text -> Parser T.Text
        getstring str = do
          start <- takeWhileP (Just "Regular String") regularChar
          la <- lookAhead (optional anySingle)
          case la of
            Just '\\' -> do
              c <- escapeHandler
              getstring (str <> T.snoc start c)
            _    -> return (str <> start)

        regularChar :: Char -> Bool
        regularChar c = isPrint c && c /= '\\' && c /= '"'

        escapeHandler :: Parser Char
        escapeHandler = do
          void (char '\\') -- consumes the '\' character
          lookAhead (optional anySingle) >>= \case
            Just 'n'  -> anySingle $> '\n'
            Just 't'  -> anySingle $> '\t'
            Just '"'  -> anySingle $> '"'
            Just '^'  -> controlChar
            Just '\\' -> anySingle $> '\\'
            Just x | isDigit x -> asciiNumber
            _         -> satisfy regularChar <|> escapeHandler
                -- space1 *> char '\\' *> (satisfy regularChar <?> "printable character") <|> escapeHandler

        controlChar :: Parser Char
        controlChar = do
            void $ char '^'
            x <- char  '@' <|>
                 char' 'G' <|>
                 char' 'H' <|>
                 char' 'I' <|>
                 char' 'J' <|>
                 char' 'K' <|>
                 char' 'L' <|>
                 char' 'M' <|>
                 char' 'Z'
            case toUpper x of
                '@' -> pure '\0'
                'G' -> pure '\a'
                'H' -> pure '\b'
                'I' -> pure '\t'
                'J' -> pure '\n'
                'K' -> pure '\v'
                'L' -> pure '\f'
                'M' -> pure '\r'
                _   -> pure '\032'

        asciiNumber :: Parser Char
        asciiNumber = do
          num <- L.decimal <?> "ascii value"
          if num > 127 then
            fail $ "Illegal ascii value: "++show num
          else
            return (chr num)

    identifier :: Parser String
    identifier = do
      start <- getSrcPos
      s     <- satisfy isAlpha
      name  <- Data.Text.unpack <$> takeWhileP (Just "identifier") (\x -> isAlphaNum x || x == '_')
      if name `elem` keywords then
        fail $ "keyword found: "++show name++"@"++show start++", when identifier expected."
      else
        return (s:name)

    operators :: [[Operator Parser Exp]]
    operators = [[minus],
      [binary InfixL "*" Times, binary InfixL "/" Divide], [binary InfixL "+" Plus, binary InfixL "-" Minus],
      [binary InfixN ">" Gt, binary InfixN ">=" Ge, binary InfixN "<" Lt, binary InfixN "<=" Le],
      [binary InfixN "=" Eq, binary InfixN "<>" Neq],
      [binary InfixL "&" LAnd], [binary InfixL "|" LOr]]

    operator :: T.Text -> Parser ()
    -- try backtracking here to check <= />= after this fails
    operator ">" = void $ try $ symbol ">" <* notFollowedBy "="
    operator "<" = void $ try $ symbol "<" <* notFollowedBy (char '=' <|> char '>')
    operator op  = void $ symbol op

    minus :: Operator Parser Exp
    minus = Prefix $ do
      start <- getSrcPos
      operator "-"
      return $ \x -> --x is the exp with argument value
        let end = getEnd x in UnopExp x (Range start end)

    binary :: (Parser (Exp -> Exp -> Exp) -> Operator Parser Exp) -> T.Text -> Binop -> Operator Parser Exp
    binary typ name f = typ  $ do
      operator name
      return $ \e1 e2 ->
        let start = getStart e1 in
          let end = getEnd e2 in
            BinopExp e1 f e2 (Range start end)

    assignParser :: Parser Exp
    assignParser = undefined
    
    lvalue :: Parser Var
    lvalue =  do
      start <- getSrcPos
      id    <- lexeme identifier
      end   <- getSrcPos
      lvalue' (SimpleVar id (Range start end))
      where
        lvalue' :: Var -> Parser Var
        lvalue' v = do 
          lookAhead (optional anySingle) >>= \case
            Just '.' -> fieldId v
            Just '[' -> subscriptId v
            _        -> return v
        
        fieldId :: Var -> Parser Var
        fieldId v = do
          start <- getSrcPos
          void (symbol ".")
          id    <- lexeme identifier
          end   <- getSrcPos
          lvalue' $ FieldVar v id (Range start end)

        subscriptId :: Var -> Parser Var
        subscriptId v = do
          start <- getSrcPos
          exp   <- brackets expr
          end   <- getSrcPos
          lvalue' $ SubscriptVar v exp (Range start end)

    term :: Parser Exp
    term = try strparse
      <|> annotate (IntExp <$> lexeme integer)
      <|> annotate (symbol "nil" $> NilExp)
      <|> assignParser

    expr :: Parser Exp
    expr = undefined




