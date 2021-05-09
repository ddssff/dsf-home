-- Example of a haskell parser for the output of extractbb:

import Appraisal.Image (readRationalMaybe)
import Data.Char (isSpace)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec
import Text.Parsec.String

-- example:

extractbbExample :: String
extractbbExample =
    unlines ["%%Title: /srv/appraisalscribe-development/images/9b/9b2f815d2eb17192da83402562e88cc0.jpg",
             "%%Creator: extractbb 20150315",
             "%%BoundingBox: 0 0 256 972",
             "%%HiResBoundingBox: 0.000000 0.000000 256.000000 972.000000",
             "%%CreationDate: Sat Jan 27 11:43:31 2018"]
pnmfileExample :: String
pnmfileExample = unlines ["stdin:\tPPM raw, 256 by 972  maxval 255"]

type BBox = (Int, Int, Int, Int)
type HBBox = (Rational, Rational, Rational, Rational)

main :: IO ()
main = do
  putStrLn . show $ parse parseExtractBBOutput "example" extractbbExample
  putStrLn . show $ parse (catMaybes <$> many ((Just <$> parsePnmfileOutput) <|> (const Nothing <$> anyChar))) "example" pnmfileExample


ignoreUntil :: Parsec String () a -> Parsec String () (Maybe r) -> Parsec String () (Maybe r)
ignoreUntil p end = end <|> ((const Nothing <$> p) >> ignoreUntil p end)

data Format = Binary | Gray | Color deriving Show
data RawOrPlain = Raw | Plain deriving Show
data Pnmfile = Pnmfile Format RawOrPlain (Integer, Integer, Maybe Integer) deriving Show

parsePnmfileOutput :: Parsec String () Pnmfile
parsePnmfileOutput =
  catMaybes <$> many ((Just <$> go) <|> (const Nothing <$> anyChar))
  where
    go = do
      _ <- char 'P'
      format <- (\c -> case c of
                         'B' -> Binary
                         'G' -> Gray
                         'P' -> Color) <$> oneOf "BGP"
      _ <- string "M "
      rawOrPlain <- (\s -> case s of
                             "plain" -> Plain
                             "raw" -> Raw) <$> (string "plain" <|> string "raw")
      _ <- string ", "
      w <- many1 digit
      _ <- string " by "
      h <- many1 digit
      _ <- spaces
      mv <- optionMaybe (string "maxval " >> many1 digit)
      newline
      return $ Pnmfile format rawOrPlain (read w, read h, fmap read mv)

{-
  From the pnmfile source:
	case PBM_FORMAT:  printf( "PBM plain, %d by %d\n", cols, rows );
	case RPBM_FORMAT: printf( "PBM raw, %d by %d\n", cols, rows );
	case PGM_FORMAT:  printf( "PGM plain, %d by %d  maxval %d\n", cols, rows, maxval );
	case RPGM_FORMAT: printf( "PGM raw, %d by %d  maxval %d\n", cols, rows, maxval );
	case PPM_FORMAT:  printf( "PPM plain, %d by %d  maxval %d\n", cols, rows, maxval );
	case RPPM_FORMAT: printf( "PPM raw, %d by %d  maxval %d\n", cols, rows, maxval );

-}


parseExtractBBOutput :: Parsec String () ((Integer, Integer, Integer, Integer),
                                          (Rational, Rational, Rational, Rational))
parseExtractBBOutput = do
  title
  creator
  bb <- boundingBox
  hbb <- hiResBoundingBox
  creationDate
  return (bb, hbb)
    where
      title :: Parsec String () String
      title = string "%%Title:" >> spaces >> many (noneOf "\n") >>= \r -> newline >> return r

      creator = string "%%Creator:" >> spaces >> many (noneOf "\n") >> newline

      boundingBox :: Parsec String () (Integer, Integer, Integer, Integer)
      boundingBox = do
        _ <- string "%%BoundingBox:"
        spaces
        l <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        t <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        r <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        b <- many1 (satisfy (not . isSpace))
        newline
        return (read l, read t, read r, read b)

      hiResBoundingBox :: Parsec String () (Rational, Rational, Rational, Rational)
      hiResBoundingBox = do
        _ <- string "%%HiResBoundingBox:"
        spaces
        l <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        t <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        r <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        b <- many1 (satisfy (not . isSpace))
        newline
        return (readRational l, readRational t, readRational r, readRational b)

      creationDate :: Parsec String () ()
      creationDate = string "%%CreationDate:" >> many (noneOf "\n") >> newline >> return ()

readRational :: String -> Rational
readRational s = fromMaybe (error $ "read " ++ show s ++ " :: Rational") (readRationalMaybe s)
