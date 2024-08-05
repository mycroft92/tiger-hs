{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Errors where
    import Data.Text (Text, unpack)
    import Control.Applicative hiding (some)
    import Data.List (intercalate)
    import Data.Set (Set)

    import Data.Void
    import Text.Megaparsec ( ShowErrorComponent,
        Parsec,
        parseErrorTextPretty,
        ErrorFancy (..),
    --   ErrorFail,
    --   ErrorCustom,
    --   ErrorIndentation,
        ErrorItem,
        ParseError (..), MonadParsec (parseError), observing, fancyFailure)
    import Text.Megaparsec.Char
    import qualified Data.Set as Set
    import AST 
    type Parser = Parsec ParserErrors Text
    

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

    data ParserErrors = 
        TrivialWithLocation
        [String] -- position stack
        (Maybe (ErrorItem Char))
        (Set (ErrorItem Char))
        | FancyWithLocation
            [String] -- position stack
            (ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
        deriving (Eq, Ord, Show)

    instance ShowErrorComponent ParserErrors where
    showErrorComponent (TrivialWithLocation stack us es) =
        parseErrorTextPretty (TrivialError @Text @Void undefined us es)
        ++ showPosStack stack
    showErrorComponent (FancyWithLocation stack cs) =
        parseErrorTextPretty (FancyError @Text @Void undefined (Set.singleton cs))
        ++ showPosStack stack

    showPosStack :: [String] -> String
    showPosStack = intercalate ", " . fmap ("in " ++)
    

    inside :: String -> Parser a -> Parser a
    inside location p = do
        r <- observing p
        case r of
            Left (TrivialError _ us es) ->
                fancyFailure . Set.singleton . ErrorCustom $
                TrivialWithLocation [location] us es
            Left (FancyError _ xs) -> do
                let f (ErrorFail msg) = ErrorCustom $ 
                        FancyWithLocation [location] (ErrorFail msg)
                    f (ErrorIndentation ord rlvl alvl) = ErrorCustom $
                        FancyWithLocation [location] (ErrorIndentation ord rlvl alvl)
                    f (ErrorCustom (TrivialWithLocation ps us es)) = ErrorCustom $
                        TrivialWithLocation (location:ps) us es
                    f (ErrorCustom (FancyWithLocation ps cs)) = ErrorCustom $
                        FancyWithLocation (location:ps) cs
                fancyFailure (Set.map f xs)
            Right x -> return x
        -- showErrorComponent (NotValidExp e) = show 

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f