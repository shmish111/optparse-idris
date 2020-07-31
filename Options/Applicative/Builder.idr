-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Builder

import public Text.PrettyPrint.Leijen
import        Options.Applicative.Types
import        Control.Monad.Trans
import        Control.Lens


public export
optRdr : Lens' (Option x a) (OptReader x a)
optRdr (Mor f) = Mor (\a => case a of
      (Opt props rdr) => (Opt props) <$> f rdr
    )

public export
optProps : Lens' (Option x a) (OptProperties)
optProps (Mor f) = Mor (\a => case a of
      (Opt props rdr) => (flip Opt rdr) <$> f props
    )

public export
interface HasMeta d where
  meta : { f : Type -> Type } -> Functor f => LensLike' f d String

public export
HasMeta (OptReader OptionParams a) where
  meta (Mor f) = Mor (\a => case a of
          (OptionReader n p m) => (OptionReader n p) <$> f m
        )

public export
HasMeta (OptReader ArgParams a) where
  meta (Mor f) = Mor (\a => case a of
          (ArgReader p m) => (ArgReader p) <$> f m
        )

public export
HasMeta (OptReader CmdParams a) where
  meta (Mor f) = Mor (\a => case a of
          (CmdReader ps m) => (CmdReader ps) <$> f m
        )

public export
HasMeta (OptReader x a) => HasMeta (Option x a) where
  meta = optRdr . meta


public export
interface HasName d where
  names : { f : Type -> Type } -> Functor f => LensLike' f d (List OptName)

public export
HasName (OptReader OptionParams a) where
  names (Mor f) = Mor (\a => case a of
         (OptionReader n p m) => (\n' => OptionReader n' p m) <$> (f n)
        )

public export
HasName (OptReader FlagParams a) where
  names (Mor f) = Mor (\a => case a of
         (FlagReader n d) => (\n' => FlagReader n' d) <$> f n
        )

public export
HasName (OptReader x a) => HasName (Option x a) where
  names = optRdr . names

public export
short : HasName d => Char -> d -> d
short c = over names (\ns => ShortName c :: ns)

public export
long : HasName d => String -> d -> d
long c = over names (\ns => LongName c :: ns)

public export
interface HasSubCommands (d : Type ) a where
  cmds : { f : Type -> Type } -> Functor f => LensLike' f d (List (String, (Parser a)))

public export
HasSubCommands (OptReader CmdParams a) a where
  cmds (Mor f) = Mor (\a => case a of
          (CmdReader ps m) => flip CmdReader m <$> f ps
        )

public export
HasSubCommands (Option CmdParams a) a where
  cmds = optRdr . cmds

public export
cmd : HasSubCommands d a => String -> Parser a -> d -> d
cmd n p = over cmds (\ns => (n,p) :: ns)

public export
interface HasHelp d where
  help : { f : Type -> Type } -> Functor f => LensLike' f d Doc

public export
HasHelp OptProperties where
  help (Mor f) = Mor (\a => case a of
         (MkOptProperties vis hdoc) => MkOptProperties vis <$> f hdoc
       )

public export
HasHelp (Option x a) where
  help = optProps . help

public export
interface HasVisibility d where
  visibility : { f : Type -> Type } -> Functor f => LensLike' f d Visibility

public export
HasVisibility OptProperties where
  visibility (Mor f) = Mor (\a => case a of
         (MkOptProperties vis hdoc) => (\vis' => MkOptProperties vis' hdoc) <$> f vis
        )

public export
HasVisibility (Option x a) where
  visibility = optProps . visibility

public export
hide : HasVisibility d => d -> d
hide d = d |> visibility .~ Hidden

public export
internal : HasVisibility d => d -> d
internal d = d |> visibility .~ Internal

public export
interface HasValue ( d : Type -> Type ) where
  value : { a : Type } -> { b : Type } -> { f : Type -> Type } -> Functor f => LensLike f (d a) (d b) a b

public export
HasValue (OptReader FlagParams) where
  value (Mor f) = Mor (\a => case a of
         (FlagReader n d) => (\d' => FlagReader n d') <$> f d
        )

public export
HasValue (Option FlagParams) where
  value (Mor f) = Mor (\a => case a of
         (Opt props (FlagReader n d)) => (\d' => Opt props $ FlagReader n d') <$> f d
        )

public export
defProps : OptProperties
defProps = MkOptProperties Visible empty

public export
option : (String -> Either ParseError a) -> (Option OptionParams a -> Option OptionParams a) -> Parser a
option rdr f = OptP  (f $ Opt defProps (OptionReader [] rdr "OPT"))

public export
strOption : (Option OptionParams String -> Option OptionParams String) -> Parser String
strOption = option Right

public export
arg : (String -> Either ParseError a) -> (Option ArgParams a -> Option ArgParams a) -> Parser a
arg rdr f = OptP (f $ Opt defProps (ArgReader rdr "ARG"))

public export
strArg : (Option ArgParams String -> Option ArgParams String) -> Parser String
strArg = arg Right

public export
flag : (Option FlagParams Bool -> Option FlagParams a) -> Parser a
flag f = OptP (f $ Opt defProps (FlagReader [] True))

public export
subparser : (Option CmdParams a -> Option CmdParams a) -> Parser a
subparser f = OptP (f $ Opt defProps (CmdReader [] "COMMAND"))

-- --------------------------------------------------------------------- [ EOF ]
