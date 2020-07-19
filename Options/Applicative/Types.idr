-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Types

import public Text.PrettyPrint.Leijen
import        Control.Monad.Trans
import        Control.Lens

public export
data ParamType = OptionParams
               | FlagParams
               | ArgParams
               | CmdParams

public export
data Visibility = Visible
                | Hidden
                | Internal

public export
Eq Visibility where
  (==) Visible Visible   = True
  (==) Hidden Hidden     = True
  (==) Internal Internal = True
  (==) _ _               = False

public export
data OptProperties = MkOptProperties Visibility Doc

public export
data OptName : Type where
  ShortName : Char   -> OptName
  LongName  : String -> OptName

public export
Eq OptName where
  (==) (ShortName a) (ShortName b) = a == b
  (==) (LongName  a) (LongName  b) = a == b
  _  == _ = False

public export
data ParseError : Type where
  ErrorMsg : String -> ParseError

mutual
  public export
  data OptReader : ParamType -> a -> Type where
    OptionReader : List OptName -> (String -> Either ParseError a) -> String -> OptReader OptionParams a
    FlagReader   : List OptName -> a                                         -> OptReader FlagParams a
    ArgReader    :                 (String -> Either ParseError a) -> String -> OptReader ArgParams a
    CmdReader    : List (String, Parser a)                         -> String -> OptReader CmdParams a

  public export
  Functor (OptReader ps) where
    map f (OptionReader n p m) = OptionReader n (map f . p) m
    map f (FlagReader n d)     = FlagReader n (f d)
    map f (ArgReader p m)      = ArgReader (map f . p) m
    map f (CmdReader ps m)     = assert_total $ CmdReader ((\(n,p) => (n, map f p)) <$> ps) m

  public export
  data Option : ParamType -> (a : Type) -> Type where
    Opt : OptProperties -> OptReader ps a -> Option ps a

  public export
  Functor (Option ps) where
    map f (Opt props rdr) = Opt props (map f rdr)

  public export
  data Parser : (a : Type) -> Type where
    NilP : Maybe a -> Parser a
    OptP : {ps : ParamType} -> Option ps a -> Parser a
    AppP : Parser (x -> a) -> Parser x -> Parser a
    AltP : Parser a -> Parser a -> Parser a

  public export
  Functor Parser where
    map f (NilP x) = NilP (map f x)
    map f (OptP x) = OptP (map f x)
    map f (AppP ff a) = AppP (map (f .) ff) a
    map f (AltP a b) = AltP (map f a) (map f b)

public export
Applicative Parser where
  pure  = NilP . Just
  (<*>) = AppP

public export
Alternative Parser where
  empty = NilP Nothing
  (<|>) = AltP

-- --------------------------------------------------------------------- [ EOF ]
