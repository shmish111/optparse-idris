-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Chunk

import public Text.PrettyPrint.Leijen
import Data.Maybe

%default total

public export
data Chunk a = MkChunk (Maybe a)

public export
unChunk : Chunk a -> Maybe a
unChunk (MkChunk a) = a

public export
Functor Chunk where
  map f (MkChunk x) = MkChunk $ map f x

public export
Applicative Chunk where
  pure = MkChunk . pure
  (MkChunk f) <*> (MkChunk x) = MkChunk (f <*> x)

public export
Monad Chunk where
  (MkChunk m) >>= f = MkChunk $ m >>= unChunk . f

public export
chunked : (a -> a -> a)
        -> Chunk a -> Chunk a -> Chunk a
chunked _ (MkChunk Nothing) y = y
chunked _ x (MkChunk Nothing) = x
chunked f (MkChunk (Just x)) (MkChunk (Just y)) = MkChunk (Just (f x y))

public export
extractChunk : Monoid a => Chunk a -> a
extractChunk (MkChunk x) = fromMaybe neutral x

public export
(<+>) : Chunk Doc -> Chunk Doc -> Chunk Doc
(<+>) = chunked (|++|)

public export
isEmptyChunk : Chunk a -> Bool
isEmptyChunk (MkChunk Nothing)  = True
isEmptyChunk (MkChunk (Just _)) = False


public export
tabulate' : Int -> List (Doc, Doc) -> Chunk Doc
tabulate' _ [] = MkChunk Nothing
tabulate' size table = pure $ vcat
  [ indent 2 (fillBreak size key |+| value)
  | (key, value) <- table ]

-- | Display pairs of strings in a table.
public export
tabulate : List (Doc, Doc) -> Chunk Doc
tabulate = tabulate' 24

-- --------------------------------------------------------------------- [ EOF ]
