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
    import Text.Megaparsec.Debug (dbg)
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
    parse fname src = case runParser (expr <* eof) fname src of
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

    symbol' :: T.Text -> Parser String
    symbol' x = T.unpack <$> L.symbol space x

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme space

    brackets :: Parser a -> Parser a
    brackets x = space *> between (symbol "[") (symbol "]") x

    curly :: Parser a -> Parser a
    curly x = space *> between (symbol "{") (symbol "}") x

    parens :: Parser a -> Parser a
    parens x = space *> between (symbol "(") (symbol ")") x

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
    identifier = space *> do
      start <- getSrcPos
      s     <- satisfy isAlpha
      name  <- Data.Text.unpack <$> takeWhileP (Just "identifier") (\x -> isAlphaNum x || x == '_')
      if (s:name) `elem` keywords then
        fail $ "keyword found: "++show (s:name)++"@"++show start++", when identifier expected."
      else
        return (s:name)

    keyword :: String -> Parser String
    keyword t = space *> (do
      start <- getSrcPos
      s     <- satisfy isAlpha
      name  <- Data.Text.unpack <$> takeWhileP (Just "keyword") (\x -> isAlphaNum x || x == '_')
      if (s:name) == t then
        return (s:name)
      else
        fail $ "identifier found: "++show (s:name)++"@"++show start++", when keyword "++show t ++" expected.") <* space

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
      void space
      operator name
      return $ \e1 e2 ->
        let start = getStart e1 in
          let end = getEnd e2 in
            BinopExp e1 f e2 (Range start end)

    -- assignParser :: Parser Exp
    -- assignParser = undefined

    lvalue :: Parser Var
    lvalue =  do
      void space
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

    ite :: Parser Exp
    ite = annotate $ do
      void (keyword "if" <?> "if")
      cond <- expr <?> "condition exp"
      void (keyword "then")
      texp <- expr <?> "true exp"
      void (keyword "else")
      fexp <- expr <?> "false exp"
      return $ IfExp cond texp (Just fexp)

    ite' :: Parser Exp
    ite' = annotate $  do
      void (keyword "if" <?> "if")
      cond <- expr <?> "condition exp"
      void (keyword "then")
      texp <- expr <?> "true exp"
      return $ IfExp cond texp Nothing

    ifexp :: Parser Exp
    ifexp = try ite <|> ite'

    whileexp :: Parser Exp
    whileexp = annotate $ do
      void (keyword "while")
      cond <- lexeme expr <?> "loop condition"
      void (keyword "do")
      WhileExp cond <$> lexeme expr <?> "loop body"

    forexp  :: Parser Exp
    forexp = annotate $ do
      void (keyword "for")
      id    <- lexeme identifier <?> "loop variable"
      void (symbol ":=")
      start <- lexeme expr <?> "loop start"
      void (keyword "to")
      end   <- lexeme expr <?> "loop end"
      void (keyword "do")
      body <- lexeme expr <?> "loop body"
      return $ ForExp id False start end body

    letexp :: Parser Exp
    letexp = annotate $ do
      void (keyword "let")
      letdecs <- sepBy1 dec space1
      void (keyword "in")
      seqstart <- getSrcPos
      expseq   <- sepBy1 expr (symbol ";")
      seqend   <- getSrcPos
      void (keyword "end")
      return $ LetExp letdecs (SeqExp expseq (Range seqstart seqend))


    term :: Parser Exp
    term = try strparse
      <|> try (annotate (IntExp <$> lexeme integer))
      <|> try (annotate (keyword "nil" $> NilExp))
      <|> try (annotate (SeqExp <$> lexeme (parens (sepBy1 expr (symbol ";")))))
      -- <|> try (parens expr)
      -- record, callexp and lvalue all start with same identifier rule.
      <|> try (annotate callexp)
      <|> try (annotate recordexp)
      <|> try (annotate arrayexp)
      <|> try (annotate assignexp)
      <|> try break
      <|> try whileexp
      <|> try forexp
      <|> try ifexp
      <|> try letexp
      <|> (VarExp <$> lvalue)
      

      where
        callexp :: Parser (Range -> Exp)
        callexp = do
          id   <- lexeme identifier <?> "Function call identifier"
          exps <- lexeme $ parens (sepBy expr (symbol ","))
          return $ CallExp id exps

        recordexp :: Parser (Range -> Exp)
        recordexp = do
          tyid <- lexeme identifier <?> "Record type name"
          vals <- curly (sepBy fieldexp (symbol ","))
          return $ RecordExp tyid vals

        fieldexp :: Parser Field
        fieldexp = space *> annotate (do
          id  <- lexeme identifier <?> "record field identifier"
          void (symbol "=" <?> "record assignment")
          Field id <$> expr)

        arrayexp :: Parser (Range -> Exp)
        arrayexp = do
          id <- lexeme identifier <?> "Array identifier"
          exp1 <- brackets expr <?> "Subscript exp"
          void (keyword "of")
          exp2 <- lexeme expr <?> "Value exp"
          return $ ArrayExp id exp1 exp2

        assignexp :: Parser (Range -> Exp)
        assignexp = do
          lval <- lexeme lvalue <?> "assign Lvalue"
          void (symbol ":=")
          AssignExp lval <$> lexeme expr

        break :: Parser Exp
        break = annotate $ void (keyword "break") $> BreakExp

      -- <|> assignParser

    expr :: Parser Exp
    expr = space *> makeExprParser term operators

    -- id : type 
    typeField :: Parser RecField
    typeField = annotate $ do
      _  <- space
      id <- lexeme identifier <?> "param name"
      void (symbol ":")
      tyid <- lexeme identifier <?> "param type"
      return $ RecField id False tyid

    typeFields :: Parser [RecField]
    typeFields = dbg "type fields" $ sepBy typeField (symbol ",")


    tydecs :: Parser Dec
    tydecs =  annotate $ do
      a <- sepBy1 tydec space1 <?> "Type declarations"
      return $ TypeDec a
      where
        tydec :: Parser TypeD
        tydec =  annotate (parser <?> "type declaration")
          where
            parser = do
              void (keyword "type")
              tyid <- lexeme identifier
              void $ symbol "="
              lookAhead (optional $ space *> keyword "array" <|> identifier <|> symbol' "{") >>= \case
                Just "array" -> arrayType tyid
                Just "{"     -> fieldDecl tyid
                Just  _      -> do
                  ty <- annotate (NameTy <$> lexeme identifier)
                  return $ TypeD tyid ty
                Nothing      -> fail $ show tyid ++ ":: Not a valid type declaration!"

            arrayType :: String -> Parser (Range -> TypeD)
            arrayType x = do
              start <- getSrcPos
              void (keyword "array")
              void (keyword "of")
              arrty <- lexeme identifier <?> "array type identifier"
              TypeD x . ArrayTy arrty . Range start <$> getSrcPos

            fieldDecl :: String -> Parser (Range -> TypeD)
            fieldDecl x = do
              start <- getSrcPos
              xs    <- between (symbol "{") (symbol "}") typeFields
              TypeD x . RecordTy xs . Range start <$> getSrcPos

    vardec :: Parser Dec
    vardec = annotate (do
      void (keyword "var")
      varid <- lexeme identifier
      void (space *> char ':')
      lookAhead (optional (char '=')) >>= \case
        Just {} -> withoutType varid
        Nothing -> withType varid
      )
      where
        withType varid = do
          tyid <- lexeme identifier
          void (symbol ":=")
          exp <- lexeme expr
          return $ VarDec varid False (Just tyid) exp

        withoutType varid = do
          void (char '=')
          exp <- lexeme expr
          return $ VarDec varid False Nothing exp

    fundecs :: Parser Dec
    fundecs = annotate (FunctionDec <$> sepBy1 fundec space1 <?> "function declaration block")
      where
        fundec :: Parser FunDec
        fundec = annotate (do
          void (dbg "func entry" (keyword "function" <?> "function keyword"))
          id <- dbg "func name" (lexeme identifier <?> "function name")
          params <- dbg "params" (parens typeFields <?> "function arguments")
          lookAhead (space *> optional anySingle) >>= \case
            Just ':' -> do
              void (symbol ":")
              retid <- lexeme identifier <?> "return type"
              void (symbol "=")
              FunDec id params (Just retid) <$> parseExp
            Just '=' -> FunDec id params Nothing <$> parseExp
            _ -> fail "Expected '=' or ':' after function declaration."
          )
          where
            parseExp :: Parser Exp
            parseExp = dbg "parsing body" $ do
              void (space *> symbol "=")
              expr



    dec  :: Parser Dec
    -- dec = space *> try (annotate (TypeDec . (:[]) <$> tydec)) 
    --   <|> try (annotate (FunctionDec . (:[]) <$> fundec))
    --   <|> try vardec 

    dec = space *> try vardec <|> try fundecs <|> try tydecs
      -- try (annotate (TypeDec <$> sepBy1 tydec space1))<|>
      -- try (annotate (FunctionDec  <$>sepBy1 fundec space1))<|>
      -- try vardec


    exprtest1 :: Either (ParseErrorBundle T.Text Void) Exp
    exprtest1 = Text.Megaparsec.parse expr "" " (a+b) = c-d*(5+2)"

    exprtest2 :: Either (ParseErrorBundle T.Text Void) Exp
    exprtest2 = Text.Megaparsec.parse expr "" "      (    a  + b = b-c*5 ;break    )"

    exprtest3 :: Either (ParseErrorBundle T.Text Void) Exp
    exprtest3 = Text.Megaparsec.parse expr "" "let var any : int := any{any=0} var i := readint(any) in nil end"

    dectest :: Either (ParseErrorBundle T.Text Void) Dec
    dectest = Text.Megaparsec.parse dec "" src3--" var any : any := any{any=0} var i := readint(any)"

    dectest2 :: Either (ParseErrorBundle T.Text Void) [Dec]
    dectest2 = runParser (space *>  some dec) "" src3 --" type intlist = array of int    type strlist = array of string var any : any := any{any=0} var i := readint(any)"
    
    functest :: Either (ParseErrorBundle T.Text Void) Dec
    functest = Text.Megaparsec.parse fundecs "" "function printboard() = (for i := 0 to N-l do (for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\"); print(\"\n\")); print(\"\n\"))"

    fortest :: Either (ParseErrorBundle T.Text Void) Exp
    fortest = Text.Megaparsec.parse forexp "" "for i := 0 to N-l do (for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\"); print(\"\n\")); print(\"\n\")"

    exprtest5 :: Either (ParseErrorBundle T.Text Void) Exp
    exprtest5 = Text.Megaparsec.parse letexp "" src2

    src3 :: T.Text
    src3 = "var N := 8 type intArray = array of int var row := intArray [ N ] of 0 var col := intArray [ N ] of 0 var diagl := intArray [N+N-l] of 0 var diag2 : = intArray [N+N-l] of 0 function printboard() = (for i := 0 to N-l do (for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\"); print(\"\n\")); print(\"\n\"))"

    src2 :: T.Text
    src2 = "let var N := 8 type intArray = array of int var row := intArray [ N ] of 0 var col := intArray [ N ] of 0 var diagl := intArray [N+N-l] of 0 var diag2 : = intArray [N+N-l] of 0 function printboard() = (for i := 0 to N-l do (for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\"); print(\"\n\")); print(\"\n\")) in try(0) end "
-- r := 0 to N-l
-- if row[r]=0 & diagl[r+c]=0 & diag2[r+7-c]=0
-- then (row[r]:=l; diagl[r+c]:=1; diag2[r+7-c]:=1;
-- col[c]:=r;
-- try(c+1);
-- row[r]:=0; diagl[r+c]:=0; diag2[r+7-c]:=0)"

    src :: T.Text
    src = "let\
\ var N := 8\
\ type intArray = array of int\
\ var row := intArray [ N ] of 0\
\ var col := intArray [ N ] of 0\
\ var diagl := intArray [N+N-l] of 0 var diag2 : = intArray [N+N-l] of 0\
\ function printboard() = (for i := 0 to N-l\
\ do (for j := 0 to N-l\
\ do print(if col[i]=j then \" 0\" else \" .\");\
\ print(\"\\n\")); print(\"\\n\"))\
\ function try(c:int) = if c=N\
\ then printboardO\
\ else for do\
\ in try(O) end\
\ r := 0 to N-l\
\ if row[r]=0 & diagl[r+c]=0 & diag2[r+7-c]=0\
\ then (row[r]:=l; diagl[r+c]:=1; diag2[r+7-c]:=1;\
\ col[c]:=r;\
\ try(c+1);\
\ row[r]:=0; diagl[r+c]:=0; diag2[r+7-c]:=0)"