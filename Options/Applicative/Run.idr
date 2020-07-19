-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Run

import Options.Applicative.Types
import Options.Applicative.Maybe
import Control.Monad.State
import Control.Monad.Trans
import Data.List
import Data.Either


public export
data OptWord : Type where
  ParsedWord :  OptName -> Maybe String -> OptWord

public export
parseWord : String -> Maybe OptWord
parseWord s = case unpack s of
  ('-' :: '-' :: w) => Just $ case span (/= '=') w of
        (_, [])         => ParsedWord (LongName . pack $ w) Nothing
        (w', _ :: rest) => ParsedWord (LongName . pack $ w') $ Just . pack $ rest
  ('-' :: w :: [])  => Just $ ParsedWord (ShortName w) Nothing
  ('-' :: w :: rs)  => Just $ ParsedWord (ShortName w) $ Just (pack rs)
  _                 => Nothing

public export
searchParser : Parser a -> (forall r. forall g. Option g r -> MaybeT (StateT (List String) (Either ParseError)) (Parser r)) -> MaybeT (StateT (List String) (Either ParseError)) (Parser a)
searchParser (NilP x) _ = empty
searchParser (OptP o) f = f o
searchParser (AppP p1 p2) f = (<|>)
  ( (\p1' => p1' <*> p2) <$> searchParser p1 f )
  ( (\p2' => p1 <*> p2') <$> searchParser p2 f )
searchParser (AltP p1 p2) f = (<|>)
  ( searchParser p1 f )
  ( searchParser p2 f )

public export
stepParser : {a : Type} -> Parser a -> String -> MaybeT (StateT (List String) (Either ParseError)) (Parser a)
stepParser p arg = case (parseWord arg) of
  Nothing => searchParser p $ \opt => case opt of
    Opt _ (ArgReader fa _) => lift $ lift $ map pure (fa arg)
    Opt _ (CmdReader ps _) => case lookup arg ps of
      Just sub => pure sub
      Nothing  => empty
    _                      => empty
  Just (ParsedWord w wordVal) => searchParser p $ \opt => case opt of
    Opt _ (FlagReader w' a)    => case elem w w' of
      True  => do
        args <- lift $ ST (\x => pure (x, x))
        let poppedArgs  = maybe [] (\w => ("-" <+> w) :: Nil) wordVal <+> args
        lift $ ST (\y => pure ((), poppedArgs))
        lift . lift . Right . pure $ a
      False => empty
    Opt _ (OptionReader w' fa _) => case elem w w' of
      True  => do
        args <- lift $ ST (\x => pure (x, x))
        let argsWord = maybe [] (:: Nil) wordVal <+> args
        case argsWord of
          (a :: rest) => do
            lift $ ST (\y => pure ((), rest))
            lift . lift . map pure $ fa a
          _ => lift $ lift (Left $ ErrorMsg "Input required after option ")
      False => empty
    _ => empty

public export
evalParser : Parser a -> Maybe a
evalParser (NilP r) = r
evalParser (OptP _) = Nothing
evalParser (AppP p1 p2) = evalParser p1 <*> evalParser p2
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2

public export
parseError : String -> ParseError
parseError arg = ErrorMsg msg
  where
    msg : String
    msg = case unpack arg of
      ('-'::_) => "Invalid option `" ++ arg ++ "'"
      _        => "Invalid argument `" ++ arg ++ "'"

public export
runParser : {a : Type} -> Parser a -> List String -> Either ParseError (a, List String)
runParser p Nil = maybeToEither (ErrorMsg "Not enough input") $ map (\p' => (p', Nil)) (evalParser p)
runParser p args@(arg :: argt) = do
  x <- runStateT (runMaybeT $ stepParser p arg) argt
  case x of
    (Just p', args') => runParser p' args'
    _                => maybeToEither (parseError arg) $ map (\x' => (x', args)) (evalParser p)

public export
runParserFully : {a : Type} -> Parser a -> List String -> Either ParseError a
runParserFully p args = do
  (res,leftOver) <- runParser p args
  case leftOver of
    (un :: _) => Left $ parseError un
    Nil       => Right res

-- --------------------------------------------------------------------- [ EOF ]
