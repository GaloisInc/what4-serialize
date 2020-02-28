-- | Definition of the S-Expression tokens used to
-- (de)serialize What4 expressions.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module What4.Serialize.SETokens
    ( Atom(..)
    , string, ident, int, bitvec, bool, nat, real
    , string', ident'
    , printAtom
    , printSExpr
    , parseSExpr
    )
    where

import qualified Data.Foldable as F
import qualified Data.SCargot as SC
import qualified Data.SCargot.Comments as SC
import qualified Data.SCargot.Repr as SC
import qualified Data.SCargot.Repr.WellFormed as SC
import           Data.Semigroup
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Numeric.Natural ( Natural )
import qualified Text.Parsec as P
import           Text.Parsec.Text ( Parser )
import           Text.Printf ( printf )
import           Data.Ratio

import           Data.Parameterized.Some ( Some(..))
import qualified What4.BaseTypes as W4

import           Prelude

data Atom =
  AId Text
  -- ^ An identifier.
  | AStr (Some W4.StringInfoRepr) Text
  -- ^ A prefix followed by a string literal
  -- (.e.g, AStr "u" "Hello World" is serialize as `#u"Hello World"`).
  | AInt Integer
  -- ^ Integer (i.e., unbounded) literal.
  | ANat Natural
  -- ^ Natural (i.e., unbounded) literal.
  | AReal Rational
  -- ^ Real (i.e., unbounded) literal.
  | ABV Int Integer
  -- ^ Bitvector, width and then value.
  | ABool Bool
  -- ^ Boolean literal.
  deriving (Show, Eq, Ord)

type SExpr = SC.WellFormedSExpr Atom

string :: Some W4.StringInfoRepr -> Text -> SExpr
string strInfo str = SC.A $ AStr strInfo str

string' :: Some W4.StringInfoRepr -> String -> SExpr
string' strInfo str = SC.A $ AStr strInfo (T.pack str)

-- | Lift an unquoted identifier.
ident :: Text -> SExpr
ident = SC.A . AId

ident' :: String -> SExpr
ident' = SC.A . AId . T.pack


-- | Lift a quoted identifier.
-- quoted :: String -> SExpr
-- quoted = SC.A . AQuoted

-- | Lift an integer.
int :: Integer -> SExpr
int = SC.A . AInt

-- | Lift an integer.
nat :: Natural -> SExpr
nat = SC.A . ANat

-- | Lift an integer.
real :: Rational -> SExpr
real = SC.A . AReal


-- | Lift a bitvector.
bitvec :: Natural -> Integer -> SExpr
bitvec w v = SC.A $ ABV (fromEnum w) v

-- | Lift a boolean.
bool :: Bool -> SExpr
bool = SC.A . ABool


-- * Output of the S-Expression Formula language


-- | Generates the the S-expression tokens represented by the sexpr
-- argument, preceeded by a list of strings output as comments.
printSExpr :: Seq.Seq String -> SExpr -> T.Text
printSExpr comments sexpr =
  let outputFmt = SC.setIndentAmount 1 $ SC.unconstrainedPrint printAtom
  in formatComment comments <> (SC.encodeOne outputFmt $ SC.fromWellFormed sexpr)


formatComment :: Seq.Seq String -> T.Text
formatComment c
  | Seq.null c = T.empty
  | otherwise = T.pack $ unlines $ fmap formatLine (F.toList c)
  where
    formatLine l = printf ";; %s" l


printAtom :: Atom -> T.Text
printAtom a =
  case a of
    AId s -> s
    --AQuoted s -> T.pack ('\'' : s)
    AStr si s -> (stringInfoToPrefix si)<>"\""<>s<>"\""
    AInt i -> T.pack (show i)
    ANat n -> T.pack $ "#u"++(show n)
    AReal r -> T.pack $ "#r"++(show (numerator r))++"/"++(show (denominator r))
    ABV w val -> formatBV w val
    ABool b -> if b then "#true" else "#false"


formatBV :: Int -> Integer -> T.Text
formatBV w val = T.pack (prefix ++ printf fmt val)
  where
    (prefix, fmt)
      | w `rem` 4 == 0 = ("#x", "%0" ++ show (w `div` 4) ++ "x")
      | otherwise = ("#b", "%0" ++ show w ++ "b")


-- * Input and parse of the S-Expression Formula language

-- | This is only the base-level parsing of atoms.  The full language
-- parsing is handled by the base here and the Parser definitions.

parseId :: Parser Text
parseId = T.pack <$> ((:) <$> first <*> P.many rest)
  where first = P.letter P.<|> P.oneOf "@+-=<>_."
        rest = P.letter P.<|> P.digit P.<|> P.oneOf "+-=<>_."

stringInfoToPrefix :: Some W4.StringInfoRepr -> Text
stringInfoToPrefix (Some W4.Char16Repr) = "#char16"
stringInfoToPrefix (Some W4.Char8Repr) = "#char8"
stringInfoToPrefix (Some W4.UnicodeRepr) = ""


parseStrInfo :: Parser (Some W4.StringInfoRepr)
parseStrInfo =
  P.try (P.string "#char16" >> return (Some W4.Char16Repr))
  P.<|> P.try (P.string "#char8" >> return (Some W4.Char8Repr))
  P.<|> (return (Some W4.UnicodeRepr))


parseStr :: Parser (Some W4.StringInfoRepr, Text)
parseStr = do
  prefix <- parseStrInfo
  _ <- P.char '"'
  str <- concat <$> P.many ( do { _ <- P.char '\\'; c <- P.anyChar ; return ['\\',c]} P.<|> P.many1 (P.noneOf ('"':"\\")))
  _ <- P.char '"'
  return $ (prefix, T.pack str)

parseReal :: Parser Rational
parseReal = do
  _ <- P.string "#r"
  n <- (read :: (String -> Integer)) <$> P.many P.digit
  _ <- P.char '/'
  d <- (read :: (String -> Integer)) <$> P.many P.digit
  return $ n % d

parseInt :: Parser Integer
parseInt = do
  (read <$> P.many1 P.digit)
  P.<|> (*(-1)) . read <$> (P.char '-' >> P.many1 P.digit)



parseNat :: Parser Natural
parseNat = do
  _ <- P.string "#u"
  n <- P.many1 P.digit
  return $ read n

parseBool :: Parser Bool
parseBool = do 
  (P.try (P.string "#false" *> return False))
  P.<|> (P.string "#true" *> return True)
  
parseBV :: Parser (Int, Integer)
parseBV = P.char '#' >> ((P.char 'b' >> parseBin) P.<|> (P.char 'x' >> parseHex))
  where parseBin = P.oneOf "10" >>= \d -> parseBin' (1, if d == '1' then 1 else 0)

        parseBin' :: (Int, Integer) -> Parser (Int, Integer)
        parseBin' (bits, x) = do
          P.optionMaybe (P.oneOf "10") >>= \case
            Just d -> parseBin' (bits + 1, x * 2 + (if d == '1' then 1 else 0))
            Nothing -> return (bits, x)

        parseHex = (\s -> (length s * 4, read ("0x" ++ s))) <$> P.many1 P.hexDigit

parseAtom :: Parser Atom
parseAtom
  =     P.try (ANat  <$> parseNat)
  P.<|> P.try (AReal <$> parseReal)
  P.<|> P.try (AInt  <$> parseInt)
  P.<|> P.try (AId   <$> parseId)
  P.<|> P.try (uncurry AStr  <$> parseStr)
  P.<|> P.try (ABool <$> parseBool)
  P.<|> P.try (uncurry ABV <$> parseBV)
  P.<|> P.unexpected "Invalid s-expression atom"

parseSExpr :: T.Text -> Either String SExpr
parseSExpr = SC.decodeOne $ SC.asWellFormed $ SC.withLispComments (SC.mkParser parseAtom)
