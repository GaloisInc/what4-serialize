{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module What4.Serialize.FastSExpr (
  parseSExpr
  ) where

import           Control.Applicative
import qualified Control.Monad.Fail as MF
import           Data.Parameterized.Some ( Some(..) )
import           Data.Ratio ( (%) )
import qualified Data.SCargot.Repr.WellFormed as SC
import qualified Data.Text as T
import qualified Data.Void as DV
import           Numeric.Natural ( Natural )
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL
import qualified What4.BaseTypes as WT
import qualified What4.Serialize.SETokens as WST

parseSExpr :: T.Text -> Either String (SC.WellFormedSExpr WST.Atom)
parseSExpr t =
  case TM.runParser parse "<input>" t of
    Left errBundle -> Left (show errBundle)
    Right a -> Right a

type Parser a = TM.Parsec DV.Void T.Text a

parse :: Parser (SC.WellFormedSExpr WST.Atom)
parse = parseList <|> (SC.WFSAtom <$> lexeme parseAtom)

parseList :: Parser (SC.WellFormedSExpr WST.Atom)
parseList = do
  _ <- lexeme (TMC.char '(')
  items <- TM.many parse
  _ <- lexeme (TMC.char ')')
  return (SC.WFSList items)

parseId :: Parser T.Text
parseId = T.pack <$> ((:) <$> first <*> TM.many rest)
  where
    w4symbol c = c == '@'
               || c == '+'
               || c == '-'
               || c == '='
               || c == '<'
               || c == '>'
               || c == '_'
               || c == '.'
    first = TMC.letterChar <|> TM.satisfy w4symbol
    rest = TMC.alphaNumChar <|> TM.satisfy w4symbol

parseNat :: Parser Natural
parseNat = do
  _ <- TMC.string "#u"
  TMCL.decimal

parseInt :: Parser Integer
parseInt = TMCL.decimal <|> (negate <$> (TMC.char '-' *> TMCL.decimal))

parseReal :: Parser Rational
parseReal = do
  _ <- TMC.string "#r"
  n <- TMCL.decimal
  _ <- TMC.char '/'
  d <- TMCL.decimal
  return (n % d)

parseBV :: Parser (Int, Integer)
parseBV = do
  _ <- TMC.char '#'
  t <- TM.anySingle
  case t of
    'b' -> parseBin 0 0
    'x' -> parseHex
    _ -> MF.fail ("Invalid bitvector class: " ++ show t)
  where
    parseBin :: Int -> Integer -> Parser (Int, Integer)
    parseBin !nBits !value= do
      mb <- TM.optional TMC.binDigitChar
      case mb of
        Nothing -> return (nBits, value)
        Just bitChar -> parseBin (nBits + 1) (value * 2 + if bitChar == '1' then 1 else 0)
    parseHex :: Parser (Int, Integer)
    parseHex = do
      digits <- TM.some TMC.hexDigitChar
      return (length digits * 4, read ("0x" ++ digits))

parseBool :: Parser Bool
parseBool = do
  _ <- TMC.char '#'
  TM.try (TMC.string "true" *> return True) <|> (TMC.string "false" *> return False)

parseStrInfo :: Parser (Some WT.StringInfoRepr)
parseStrInfo = TM.try (TMC.string "#char16" >> return (Some WT.Char16Repr))
           <|> TM.try (TMC.string "#char8" >> return (Some WT.Char8Repr))
           <|> return (Some WT.UnicodeRepr)

parseStr :: Parser (Some WT.StringInfoRepr, T.Text)
parseStr = do
  prefix <- parseStrInfo
  _ <- TMC.char '"'
  str <- concat <$> TM.many (parseEscaped <|> TM.some (TM.noneOf ('"':"\\")))
  _ <- TMC.char '"'
  return (prefix, T.pack str)
  where
    parseEscaped = do
      _ <- TMC.char '\\'
      c <- TM.anySingle
      return ['\\', c]

parseAtom :: Parser WST.Atom
parseAtom = TM.try (uncurry WST.ABV <$> parseBV)
        <|> TM.try (WST.ABool <$> parseBool)
        <|> TM.try (WST.AId <$> parseId)
        <|> TM.try (WST.ANat <$> parseNat)
        <|> TM.try (WST.AInt <$> parseInt)
        <|> TM.try (WST.AReal <$> parseReal)
        <|> TM.try (uncurry WST.AStr <$> parseStr)

ws :: Parser ()
ws = TMCL.space TMC.space1 (TMCL.skipLineComment (T.pack ";")) empty

lexeme :: Parser a -> Parser a
lexeme = TMCL.lexeme ws
