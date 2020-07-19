module Options.Applicative.Test

import Options.Applicative

test1 : Either ParseError String
test1 = runParserFully p ["--hello", "there"]
  where
    p : Parser String
    p = strOption ( long "hello" )

test2 : String
test2 = show $ parserHelp p
    where
      p : Parser String
      p = strOption ( long "hello" )

public export covering
main : IO ()
main = case test1 of
  Left err => putStrLn "err"
  Right res => putStrLn res