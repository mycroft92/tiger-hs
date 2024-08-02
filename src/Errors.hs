module Errors where


    data LineInfo = LineCol Int Int String | Line Int String deriving (Eq)
    data ErrInfo  = ErrInfo LineInfo String deriving (Show, Eq)

    instance Show LineInfo where
        show (LineCol l c s) = "line: " ++ show l ++ " col: "  ++ show c ++ " file: "++ s
        show (Line l s)      = "line: " ++ show l ++ " file: " ++ s

    makeErr :: String -> Int -> String -> ErrInfo
    makeErr fn l = ErrInfo (Line l fn)

    makeErrCol :: String -> Int -> Int -> String -> ErrInfo
    makeErrCol fn l c = ErrInfo (LineCol l c fn)

    data Errors = 
        Unexpected
        | ResolverError String 
        | RuntimeError String 
        | ParserError String
        | ScannerError ErrInfo
        | InterpreterError String String  deriving (Show, Eq)

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f