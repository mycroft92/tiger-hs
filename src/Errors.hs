{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
--{-# LANGUAGE FlexibleContexts #-}

module Errors where
    import Data.Text (Text, unpack)
    -- import Control.Applicative hiding (some)
    import Data.List (intercalate)
    import Data.Set (Set)

    import Data.Void
    import Text.Megaparsec ( ShowErrorComponent,
        Parsec,
        parseErrorTextPretty,
        ErrorFancy (..),
        ErrorItem,
        ParseError (..),
        observing, fancyFailure, registerFancyFailure,
        getOffset,
        region

        )
    -- import Text.Megaparsec.Char
    import qualified Data.Set as Set
    -- import AST 
    type Parser = Text.Megaparsec.Parsec ParserErrors Text


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
        (Maybe (Text.Megaparsec.ErrorItem Char))
        (Set (Text.Megaparsec.ErrorItem Char))
        | FancyWithLocation
            [String] -- position stack
            (Text.Megaparsec.ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
        deriving (Eq, Ord, Show)

    instance Text.Megaparsec.ShowErrorComponent ParserErrors where
    showErrorComponent (TrivialWithLocation stack us es) =
        Text.Megaparsec.parseErrorTextPretty (Text.Megaparsec.TrivialError @Text @Void undefined us es)
        ++ showPosStack stack
    showErrorComponent (FancyWithLocation stack cs) =
        Text.Megaparsec.parseErrorTextPretty (Text.Megaparsec.FancyError @Text @Void undefined (Set.singleton cs))
        ++ showPosStack stack

    showPosStack :: [String] -> String
    showPosStack = intercalate ", " . fmap ("in " ++)


    inside :: String -> Parser a -> Parser a
    inside location p = do
        r <- Text.Megaparsec.observing p
        case r of
            Left (Text.Megaparsec.TrivialError _ us es) -> do
                -- Text.Megaparsec.registerFancyFailure . Set.singleton . Text.Megaparsec.ErrorCustom $
                --     TrivialWithLocation [location] us es
                Text.Megaparsec.fancyFailure . Set.singleton . Text.Megaparsec.ErrorCustom $
                    TrivialWithLocation [location] us es
            Left (Text.Megaparsec.FancyError _ xs) -> do
                let f (Text.Megaparsec.ErrorFail msg) = Text.Megaparsec.ErrorCustom $
                        FancyWithLocation [location] (Text.Megaparsec.ErrorFail msg)
                    f (Text.Megaparsec.ErrorIndentation ord rlvl alvl) = Text.Megaparsec.ErrorCustom $
                        FancyWithLocation [location] (Text.Megaparsec.ErrorIndentation ord rlvl alvl)
                    f (Text.Megaparsec.ErrorCustom (TrivialWithLocation ps us es)) = Text.Megaparsec.ErrorCustom $
                        TrivialWithLocation (location:ps) us es
                    f (Text.Megaparsec.ErrorCustom (FancyWithLocation ps cs)) = Text.Megaparsec.ErrorCustom $
                        FancyWithLocation (location:ps) cs
                -- Text.Megaparsec.registerFancyFailure (Set.map f xs)
                Text.Megaparsec.fancyFailure (Set.map f xs)
            Right x ->
                return x

    -- inside' :: String -> Parser a -> Parser a
    -- inside' location p = do
    --         off <- getOffset
    --         region (`f` off) p
    --     where
    --         f (Text.Megaparsec.TrivialError _ us es) o = FancyError o . Set.singleton . ErrorCustom $ TrivialWithLocation [location] us es
    --         f (Text.Megaparsec.FancyError _ xs)      o = FancyError o (Set.map f' xs)
    --         f' (ErrorFail msg) = ErrorCustom $
    --                     FancyWithLocation [location] (ErrorFail msg)
    --         f' (ErrorIndentation ord rlvl alvl) = ErrorCustom $
    --                     FancyWithLocation [location] (ErrorIndentation ord rlvl alvl)
    --         f' (ErrorCustom (TrivialWithLocation ps us es)) = ErrorCustom $
    --                     TrivialWithLocation (location:ps) us es
    --         f' (ErrorCustom (FancyWithLocation ps cs)) = ErrorCustom $
    --                     FancyWithLocation (location:ps) cs

            -- fancyFailure :: MonadParsec e s m => Set (ErrorFancy e) -- ^ Fancy error components
            --     -> m a
            -- fancyFailure xs = do
            --     o <- getOffset
            --     parseError (FancyError o xs)
    --     -- showErrorComponent (NotValidExp e) = show   

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f