module HW5.Parser
  ( parse
  ) where

import           Control.Applicative             (empty, many, some, (<|>))
import           Control.Applicative.Combinators (sepBy1)
import           Control.Monad                   (void)
import           Control.Monad.Combinators.Expr  (Operator (InfixL, InfixN, InfixR, Postfix),
                                                  makeExprParser)
import qualified Data.ByteString                 as B
import           Data.Char                       (isAlpha, isAlphaNum)
import           Data.List                       (intercalate)
import           Data.Text                       (pack)
import qualified Data.Text                       as T
import           Data.Text.Internal.Read         (hexDigitToInt)
import           Data.Void                       (Void)
import           Data.Word                       (Word8)
import           HW5.Base
import           Text.Megaparsec                 (MonadParsec (eof, lookAhead, notFollowedBy, try),
                                                  Parsec, between, choice,
                                                  manyTill, runParser, satisfy,
                                                  sepBy, (<?>))
import           Text.Megaparsec.Char            (char, hexDigitChar, space1,
                                                  spaceChar)
import qualified Text.Megaparsec.Char.Lexer      as L
import           Text.Megaparsec.Error           (ParseErrorBundle)

type Parser = Parsec Void String

skipSpaces :: Parser ()
skipSpaces = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpaces

symbol :: String -> Parser String
symbol = L.symbol skipSpaces

pByte :: Parser Word8
pByte = lexeme (do
  fir <- hexDigitChar
  sec <- hexDigitChar
  lookAhead (void spaceChar <|> void (char '#'))
  return $ fromIntegral $ hexDigitToInt fir * 16 + hexDigitToInt sec
  ) <?> "Byte in hex"

parensC :: Parser a -> Parser a
parensC = between (symbol "(") (symbol ")")

parensS :: Parser a -> Parser a
parensS = between (symbol "[") (symbol "]")

parensSC :: Parser a -> Parser a
parensSC = between (symbol "[#") (symbol "#]")

parensF :: Parser a -> Parser a
parensF = between (symbol "{") (symbol "}")

pBuiltInFun :: HiFun -> Parser HiValue
pBuiltInFun constructor = HiValueFunction constructor <$ symbol (show constructor)

pBuitlIn :: Parser HiValue
pBuitlIn = choice
  [ pBuiltInFun HiFunAdd
  , pBuiltInFun HiFunSub
  , pBuiltInFun HiFunMul
  , pBuiltInFun HiFunDiv
  , pBuiltInFun HiFunLessThan
  , pBuiltInFun HiFunGreaterThan
  , pBuiltInFun HiFunEquals
  , pBuiltInFun HiFunNotLessThan
  , pBuiltInFun HiFunNotGreaterThan
  , pBuiltInFun HiFunNotEquals
  , pBuiltInFun HiFunNot
  , pBuiltInFun HiFunAnd
  , pBuiltInFun HiFunOr
  , pBuiltInFun HiFunIf
  , pBuiltInFun HiFunLength
  , pBuiltInFun HiFunToUpper
  , pBuiltInFun HiFunToLower
  , pBuiltInFun HiFunReverse
  , pBuiltInFun HiFunTrim
  , pBuiltInFun HiFunList
  , pBuiltInFun HiFunRange
  , pBuiltInFun HiFunFold
  , pBuiltInFun HiFunPackBytes
  , pBuiltInFun HiFunUnpackBytes
  , pBuiltInFun HiFunEncodeUtf8
  , pBuiltInFun HiFunDecodeUtf8
  , pBuiltInFun HiFunZip
  , pBuiltInFun HiFunUnzip
  , pBuiltInFun HiFunSerialise
  , pBuiltInFun HiFunDeserialise
  , pBuiltInFun HiFunRead
  , pBuiltInFun HiFunWrite
  , pBuiltInFun HiFunMkDir
  , pBuiltInFun HiFunChDir
  , pBuiltInFun HiFunParseTime
  , pBuiltInFun HiFunRand
  , pBuiltInFun HiFunEcho
  , pBuiltInFun HiFunCount
  , pBuiltInFun HiFunKeys
  , pBuiltInFun HiFunValues
  , pBuiltInFun HiFunInvert
  , HiValueNull <$ symbol "null"
  , HiValueBool True <$ symbol "true"
  , HiValueBool False <$ symbol "false"
  , HiValueAction HiActionCwd <$ symbol "cwd"
  , HiValueAction HiActionNow <$ symbol "now" ]

pNumber :: Parser HiValue
pNumber = HiValueNumber . toRational <$> L.signed skipSpaces (lexeme L.scientific)

pString :: Parser HiValue
pString = HiValueString . pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

pArgs :: Parser a -> Char -> Parser [a]
pArgs elemParser endSymb = ([] <$ lookAhead (char endSymb)) <|> (elemParser `sepBy` symbol ",")

pList :: Parser HiExpr
pList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> parensS (pArgs pExpr ']')

pDict :: Parser HiExpr
pDict = HiExprDict <$> parensF (pArgs ((,) <$> (pExpr <* symbol ":") <*> pExpr) '}')

pBytes :: Parser HiValue
pBytes = HiValueBytes . B.pack <$> parensSC (many pByte)

valToExpr :: Parser HiValue -> Parser HiExpr
valToExpr = (HiExprValue <$>)

pTerm :: Parser HiExpr
pTerm = choice
  [ parensC pExpr
  , valToExpr pNumber
  , valToExpr pString
  , valToExpr pBytes
  , pList
  , pDict
  , valToExpr pBuitlIn ]

pRun :: Parser (HiExpr -> HiExpr)
pRun = HiExprRun <$ symbol "!"

pApply :: Parser (HiExpr -> HiExpr)
pApply = flip HiExprApply <$> parensC (pArgs pExpr ')')

pGetLiteral :: Parser String
pGetLiteral = intercalate "-" <$> lexeme (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-')

pGet :: Parser (HiExpr -> HiExpr)
pGet = flip HiExprApply . (: []) . HiExprValue . HiValueString . T.pack <$> (symbol "." *> pGetLiteral)

pPostfix :: Parser (HiExpr -> HiExpr)
pPostfix = foldr1 (flip (.))  <$> some (pRun <|> pApply <|> pGet)

pCompBinary :: Parser a -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
pCompBinary pName constructor = (\exprA exprB -> HiExprApply (HiExprValue $ HiValueFunction constructor) [exprA, exprB])  <$ pName

pBinary :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
pBinary name = pCompBinary $ symbol name

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ Postfix pPostfix ]
  , [ InfixL $ pBinary "*" HiFunMul
    , InfixL $ pCompBinary (try $ symbol "/" >> notFollowedBy (char '=')) HiFunDiv ]
  , [ InfixL $ pBinary "+" HiFunAdd
    , InfixL $ pBinary "-" HiFunSub ]
  , [ InfixN $ pBinary "<=" HiFunNotGreaterThan
    , InfixN $ pBinary ">=" HiFunNotLessThan
    , InfixN $ pBinary "<" HiFunLessThan
    , InfixN $ pBinary ">" HiFunGreaterThan
    , InfixN $ pBinary "==" HiFunEquals
    , InfixN $ pBinary "/=" HiFunNotEquals ]
  , [ InfixR $ pBinary "&&" HiFunAnd ]
  , [ InfixR $ pBinary "||" HiFunOr ] ]

pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorTable

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (skipSpaces *> pExpr <* eof) ""
