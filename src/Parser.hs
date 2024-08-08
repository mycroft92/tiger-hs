{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where
    import Errors -- (Errors (..), ParserErrors (..))

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
    import Text.Megaparsec hiding (State, parse)
    import Text.Megaparsec as TP
    import Text.Megaparsec.Error (errorBundlePretty)
    import AST
    import Data.Void ()
    import Data.Set as Set (map, singleton)
    import Data.Char (isAlphaNum, isAlpha, isPrint, isSpace, toUpper, chr, isDigit)
    import Data.Text (unpack)
    import Text.Megaparsec.Debug (dbg)
    -- import Control.Monad (void)
    import Data.Functor ( void, ($>) )


    -- data ParserState = ParserState {
    --     errors   :: [Errors],
    --     filename :: String
    -- }


    -- type Parser a = ParsecT Void T.Text (State ParserState) a


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

    keywords' :: [String]
    keywords' = ["for", "while", "let", "if", "type", "function", "nil", "var"]

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

    myopt :: Parser a -> String -> Parser a
    myopt f s = optional (try f) >>= \case
        Just x -> return x
        Nothing -> fail s


    strparse :: Parser Exp
    strparse = annotate (StringExp <$> strparser) <?> "string"
      where
        strparser :: Parser String
        strparser = do
          res <- between (char '"') (char '"') (getstring "")
          return $ T.unpack res

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
        regularChar c = (isPrint c || isSpace c) && c /= '\\' && c /= '"'

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

    identifier'' :: Parser String
    identifier'' = do
      start <- getSrcPos
      s     <- satisfy isAlpha
      name  <- Data.Text.unpack <$> takeWhileP (Just "identifier") (\x -> isAlphaNum x || x == '_')
      if (s:name) `elem` keywords then
        fail $ "Found: "++show (s:name)++"@"++show start++", when identifier expected."
      else
        return (s:name)

    identifier' :: Parser String
    identifier' = do
      start <- getSrcPos
      s     <- satisfy isAlpha
      name  <- Data.Text.unpack <$> takeWhileP (Just "identifier") (\x -> isAlphaNum x || x == '_')
      return (s:name)

    checkNL :: Parser ()
    checkNL = void (some (anySingleBut '\n'))
    -- error recovery parts not working for now
    checkKeywords :: T.Text  -> Parser T.Text
    checkKeywords str  = do
      space
      lookAhead (optional identifier') >>= \case
        Just val
          | val `elem` keywords' -> return str
          | otherwise -> do
              c <- anySingle
              checkKeywords (T.snoc str c)
        Nothing  -> do
          c <- anySingle
          checkKeywords (T.snoc str c)

    synchronize :: String -> ParseError T.Text ParserErrors -> Parser Exp
    synchronize location k = do
      s   <- getSrcPos
      void (synchronizeFunc k location)
      e   <- checkKeywords ""
      -- NilExp . Range s <$> getSrcPos
      parseError k
        
            -- f :: ParseError s ParserErrors -> m ()
    synchronizeFunc :: (Token s1 ~ Char, MonadParsec ParserErrors s2 m) => ParseError s1 ParserErrors -> String -> m ()
    synchronizeFunc (Text.Megaparsec.TrivialError _ us es) location = Text.Megaparsec.registerFancyFailure . Set.singleton . ErrorCustom $ TrivialWithLocation [location] us es
    synchronizeFunc (Text.Megaparsec.FancyError _ xs) location   = Text.Megaparsec.registerFancyFailure  (Set.map f' xs)
      where
            f' (ErrorFail msg) = ErrorCustom $
                        FancyWithLocation [location] (ErrorFail msg)
            f' (ErrorIndentation ord rlvl alvl) = ErrorCustom $
                        FancyWithLocation [location] (ErrorIndentation ord rlvl alvl)
            f' (ErrorCustom (TrivialWithLocation ps us es)) = ErrorCustom $
                        TrivialWithLocation (location:ps) us es
            f' (ErrorCustom (FancyWithLocation ps cs)) = ErrorCustom $
                        FancyWithLocation (location:ps) cs

    synchronizeDec :: String -> ParseError T.Text ParserErrors -> Parser Dec
    synchronizeDec location k = do
      s   <- getSrcPos
      void (synchronizeFunc k location)
      e   <- checkKeywords ""      
      --errorDec . Range s <$> getSrcPos
      parseError k


    identifier :: Parser String
    identifier = try (space *> identifier'' <* space)

    keyword' :: String -> Parser String
    keyword' t = (do
      start <- getSrcPos
      s     <- satisfy isAlpha
      name  <- Data.Text.unpack <$> takeWhileP (Just "keyword") (\x -> isAlphaNum x || x == '_')
      if (s:name) == t then
        return (s:name)
      else
        fail $ "Found: "++show (s:name)++"@"++show start++", when keyword "++show t ++" expected.") <* space

    keyword :: String -> Parser String
    keyword t = try (space *> keyword' t <* space)

    operators :: [[Operator Parser Exp]]
    operators = [[minus],
      [binary InfixL "*" Times, binary InfixL "/" Divide], [binary InfixL "+" Plus, binary InfixL "-" Minus],
      [binary InfixN ">" Gt, binary InfixN ">=" Ge, binary InfixN "<" Lt, binary InfixN "<=" Le],
      [binary InfixN "=" Eq, binary InfixN "<>" Neq],
      [binary InfixL "&" LAnd], [binary InfixL "|" LOr]]

    operator :: T.Text -> Parser ()
    -- try backtracking here to check <= />= after this fails
    operator ">" = void $ try $ space *> symbol ">" <* notFollowedBy "="
    operator "<" = void $ try $ space *> symbol "<" <* notFollowedBy (char '=' <|> char '>')
    operator op  = void $ try $ space *> symbol op

    minus :: Operator Parser Exp
    minus = Prefix $ do
      start <- getSrcPos
      try (space *> operator "-")
      return $ \x -> --x is the exp with argument value
        let end = getEnd x in UnopExp x (Range start end)

    binary :: (Parser (Exp -> Exp -> Exp) -> Operator Parser Exp) -> T.Text -> Binop -> Operator Parser Exp
    binary typ name f = typ  $ do
      operator name --I need to backtrack here otherwise it consumes space and fails term parsing
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
          exp   <- withRecovery (synchronize "subscript exp") (brackets expr)
          end   <- getSrcPos
          lvalue' $ SubscriptVar v exp (Range start end)

    ifexp :: Parser Exp
    ifexp = inside "ite" (annotate $ do
      void (keyword "if" <?> "if")
      cond <- withRecovery (synchronize "condition exp")  expr
      void (keyword "then")
      texp <-  withRecovery (synchronize "true exp") (inside "true exp" expr)
      lookAhead (optional (space *> keyword "else")) >>= \case
        Just _ ->  do
          void (space *> keyword "else")
          fexp <- withRecovery (synchronize "false exp") (inside "false exp" expr)
          return $ IfExp cond texp (Just fexp)
        Nothing -> return $ IfExp cond texp Nothing)


    -- ite' :: Parser Exp
    -- ite' = annotate $  do
    --   void (keyword "if" <?> "if")
    --   cond <- expr <?> "condition exp"
    --   void (keyword "then")
    --   texp <- expr <?> "true exp"
    --   return $ IfExp cond texp Nothing

    -- ifexp :: Parser Exp
    -- ifexp = try ite <|> ite'

    whileexp :: Parser Exp
    whileexp = annotate $ do
      void (keyword "while")
      cond <- withRecovery (synchronize "loop condition") (inside "while condition" (lexeme expr))
      void (keyword "do")
      WhileExp cond <$> withRecovery (synchronize "loop body") (inside "while body" (lexeme expr))

    forexp  :: Parser Exp
    forexp = annotate $ do
      void (keyword "for")
      id    <- lexeme identifier <?> "loop variable"
      void (symbol ":=")
      start <- withRecovery (synchronize "loop start") (inside "forloop start" (lexeme expr))
      void (keyword "to")
      end   <- withRecovery (synchronize "loop end") (inside "forloop end" expr )
      void (keyword "do")
      body <- withRecovery (synchronize "loop body") (inside "forloop body" expr)
      return $ ForExp id False start end body

    letexp :: Parser Exp
    letexp = inside "lextexp" (annotate $ do
      void (keyword "let")
      letdecs <- inside "letdecls" (some (withRecovery (synchronizeDec "letdecl") dec) <* space)
      void (keyword "in")
      seqstart <- getSrcPos
      expseq   <- inside "letexpr" (sepBy1 (withRecovery (synchronize "letexpr") expr) (symbol ";"))
      seqend   <- getSrcPos
      void (keyword "end")
      return $ LetExp letdecs (SeqExp expseq (Range seqstart seqend)))


    term :: Parser Exp
    term = (inside "string parser" strparse)
      <|> (annotate (IntExp <$> lexeme integer))
      <|> try (annotate (keyword "nil" $> NilExp))
      <|> try break
      <|> try whileexp
      <|> try (inside "forexp" forexp)
      <|> try ifexp
      <|> try letexp
      <|> try (annotate assignexp)
      <|> try (inside "seq exp" (annotate (SeqExp <$> lexeme (parens (sepBy expr (symbol ";"))))))
      -- record, callexp and lvalue all start with same identifier rule.
      <|> try (inside "callexp" (annotate callexp))
      <|> try (annotate recordexp)
      <|> try (annotate arrayexp)
      <|> (VarExp <$> lvalue)


      where
        callexp :: Parser (Range -> Exp)
        callexp = do
          id   <- lexeme identifier <?> "Function call identifier"
          exps <- inside "call params" $ lexeme $ parens (sepBy expr (symbol ","))
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
          Field id <$> inside "field exp" expr)

        arrayexp :: Parser (Range -> Exp)
        arrayexp = do
          id <- lexeme identifier <?> "Array identifier"
          exp1 <- inside "array subscript" (brackets expr <?> "Subscript exp")
          void (keyword "of")
          exp2 <- inside "array value" (lexeme expr <?> "Value exp")
          return $ ArrayExp id exp1 exp2

        assignexp :: Parser (Range -> Exp)
        assignexp = do
          lval <- lexeme lvalue <?> "assign Lvalue"
          void (symbol ":=")
          AssignExp lval <$> inside "assign exp" (lexeme expr)

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
    typeFields = inside "type fields" $ sepBy typeField (symbol ",")


    tydecs :: Parser Dec
    tydecs =  annotate $ do
      a <- some tydec  <?> "Type declarations"
      return $ TypeDec a
    
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
      ) <* space
      where
        withType varid = do
          tyid <- myopt (lexeme identifier) "Type identifier expected after ':'"
          void (symbol ":=")
          exp <- withRecovery (synchronize "var exp") (lexeme expr)
          return $ VarDec varid False (Just tyid) exp

        withoutType varid = do
          void (char '=')
          exp <- withRecovery (synchronize "var exp") (lexeme expr)
          return $ VarDec varid False Nothing exp

    fundecs :: Parser Dec
    fundecs = annotate (FunctionDec <$> some fundec <?> "function declaration block") <* space
    
    fundec :: Parser FunDec
    fundec = annotate (do
          void (inside "func entry" (keyword "function" <?> "function keyword"))
          id <- lexeme identifier <?> "function name"
          params <- inside "params" (parens typeFields <?> "function arguments")
          lookAhead (space *> optional anySingle) >>= \case
            Just ':' -> do
              void (symbol ":")
              retid <- lexeme identifier <?> "return type"
              void (symbol "=")
              FunDec id params (Just retid) <$> parseExp
            Just '=' -> FunDec id params Nothing <$> parseExp
            _ -> fail "Expected '=' or ':' after function declaration."
          ) <* space
          where
            parseExp :: Parser Exp
            parseExp = inside "function body" $ do
              void (space *> symbol "=")
              expr



    dec  :: Parser Dec
    -- dec = space *> try (annotate (TypeDec . (:[]) <$> tydec)) 
    --   <|> try (annotate (FunctionDec . (:[]) <$> fundec))
    --   <|> try vardec 

    dec = try  vardec <|> try fundecs <|> try  tydecs

    -- better error reporting with this
    dec' :: Parser Dec
    dec' = lookAhead (space *> optional identifier') >>= \case
      Just "function" -> annotate $ do
        fd <- fundec
        return $ FunctionDec [fd]
      Just "var"      -> vardec
      Just "type"     -> annotate $ do
        td <- tydec
        return $ TypeDec [td]
      _ -> fail "not a type declaration"
      -- try (annotate (TypeDec <$> sepBy1 tydec space1))<|>
      -- try (annotate (FunctionDec  <$>sepBy1 fundec space1))<|>
      -- try vardec

    -- dropUntil :: Text -> Parser a
    -- dropUntil kw = do 
    --     withRecovery 

    exprtest1 :: Either (ParseErrorBundle T.Text ParserErrors) Exp
    exprtest1 = TP.parse expr "" " (a+b) = c-d*(5+2)"

    exprtest2 :: Either (ParseErrorBundle T.Text ParserErrors) Exp
    exprtest2 = TP.parse expr "" "      (    a  + b = b-c*5 ;break    )"

    exprtest3 :: Either (ParseErrorBundle T.Text ParserErrors) Exp
    exprtest3 = TP.parse expr "" "let var any : int := any{any=0} var i := readint(any) in nil end"

    dectest :: Either (ParseErrorBundle T.Text ParserErrors) [Dec]
    dectest = TP.parse (some dec) "" src3--" var any : any := any{any=0} var i := readint(any)"

    dectest2 :: Either (ParseErrorBundle T.Text ParserErrors) [Dec]
    dectest2 = runParser (space *>  some dec) "" src3 --" type intlist = array of int    type strlist = array of string var any : any := any{any=0} var i := readint(any)"

    functest :: Either (ParseErrorBundle T.Text ParserErrors) [Dec]
    functest = TP.parse (space *>some dec) "" "var N := 8\
\ type intArray = array of int\
\ var row := intArray [ N ] of 0\
\ var col := intArray [ N ] of 0\
\ var diagl := intArray [N+N-l] of 0 var diag2 := intArray [N+N-l] of 0\
\ function printboard() = \
\    (for i := 0 to N-l do \
\       (for j := 0 to N-l\
\          do print(if col[i]=j then \" 0\" else \" .\"); print(\"\\n\"))\
\       ; print(\"\\n\"))\
\function try(c:int) = if c=N\
\ then printboard()\
\ else for r := 0 to N-l do\
\     if row[r]=0 & diagl[r+c]=0 & diag2[r+7-c]=0\
\     then (row[r]:=l; diagl[r+c]:=1; diag2[r+7-c]:=1;\
\           col[c]:=r;\
\           try(c+1);\
\     row[r]:=0; diagl[r+c]:=0; diag2[r+7-c]:=0)"

    strtest :: Either (ParseErrorBundle T.Text ParserErrors) Exp
    strtest = TP.parse strparse "" "\" \n\""

    fortest :: Either (ParseErrorBundle T.Text ParserErrors) Exp
    fortest = TP.parse forexp "" "for i := 0 to N-l do (for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\"); print(\"\\n\")); print(\"\\n\")"

    argtest :: Either (ParseErrorBundle T.Text ParserErrors) Exp
    argtest = TP.parse expr "" "( for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\") ; print(\"\n\"))"

    itetest :: Either (ParseErrorBundle T.Text ParserErrors) Exp
    itetest = TP.parse ifexp "" "if row[r]=0 & diagl[r+c]=0 & diag2[r+7-c]=0\
\     then (row[r]:=l; diagl[r+c]:=1; diag2[r+7-c]:=1;\
\           col[c]:=r;\
\           try(c+1);\
\     row[r]:=0; diagl[r+c]:=0; diag2[r+7-c]:=0) "

    exprtest5 :: Either Errors Exp
    exprtest5 = Parser.parse "" src

    src3 :: T.Text
    src3 = "var N := 8 type intArray = array of int var row := intArray [ N ] of 0 var col := intArray [ N ] of 0 var diagl := intArray [N+N-l] of 0 var diag2 := intArray [N+N-l] of 0 function printboard() = (for i := 0 to N-l do (for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\"); print(\"\\n\")); print(\"\n\"))"

    src2 :: T.Text
    src2 = "let var N := 8 type intArray = array of int var row := intArray [ N ] of 0 var col := intArray [ N ] of 0 var diagl := intArray [N+N-l] of 0 var diag2 := intArray [N+N-l] of 0 function printboard() = (for i := 0 to N-l do (for j := 0 to N-l do print(if col[i]=j then \" 0\" else \" .\"); print(\"\\n\")); print(\"\\n\")) in try(0) end "
-- r := 0 to N-l
-- if row[r]=0 & diagl[r+c]=0 & diag2[r+7-c]=0
-- then (row[r]:=l; diagl[r+c]:=1; diag2[r+7-c]:=1;
-- col[c]:=r;
-- try(c+1);
-- row[r]:=0; diagl[r+c]:=0; diag2[r+7-c]:=0)"

    src :: T.Text
    src = "let\n\
\ var N := 8\n\
\ type intArray = array of int\n\
\ var row := intArray [ N ] of 0\n\
\ var col  := intArray [ N ] of 0\n\
\ var diagl := intArray [N+N-l] of 0 var diag2 := intArray [N+N-l] of 0\
\ function printboard() = \
\    (for i := 0 to N-l do \
\       (for j := 0 to N-l\
\          do print(if col[i]=j then \" 0\" else \" .\"); print(\"\\n\"))\
\       ; print(\"\\n\"))\
\ function try(c:int) = if c=N\
\ then printboard()\
\ else for r := 0 to N-l do\
\     if row[r]=0 & diagl[r+c]=0 & diag2[r+7-c]=0\
\     then (row[r]:=l; diagl[r+c]:=1; diag2[r+7-c]:=1;\
\           col[c]:=r;\
\           try(c+1);\
\     row[r]:=0; diagl[r+c]:=0; diag2[r+7-c]:=0) in try(0) end"