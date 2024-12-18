module HW5.Pretty
  ( prettyValue
  , errMes
  ) where

import           Prettyprinter                 (Doc, Pretty (pretty), annotate,
                                                comma, enclose, encloseSep,
                                                hsep, punctuate)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (Green, Red),
                                                color)

import qualified Data.ByteString               as B
import           Data.Char                     (toLower)
import           Data.Foldable                 (Foldable (toList))
import           Data.Map                      (assocs)
import           Data.Ratio                    (denominator, numerator, (%))
import           Data.Scientific               (floatingOrInteger,
                                                fromRationalRepetendUnlimited)
import qualified Data.Text                     as T
import           Data.Word                     (Word8)
import           HW5.Base                      (HiAction (HiActionChDir, HiActionCwd, HiActionEcho, HiActionMkDir, HiActionNow, HiActionRand, HiActionRead, HiActionWrite),
                                                HiValue (HiValueAction, HiValueBool, HiValueBytes, HiValueDict, HiValueFunction, HiValueList, HiValueNull, HiValueNumber, HiValueString, HiValueTime))
import           Numeric                       (showHex)

prettyFractional :: Rational -> String
prettyFractional rat = res where
  num = numerator rat
  den = denominator rat
  (r, q) = quotRem num den

  res = if r == 0
    then show q ++ "/" ++ show den
    else show r ++ (if q < 0 then " - " else " + ") ++ prettyRational ((q * signum q) % den)

prettyRational :: Rational -> String
prettyRational rat = res where
  (sci, amount) = fromRationalRepetendUnlimited rat
  res = case amount of
    Nothing -> case (floatingOrInteger sci :: Either Float Integer) of
      Left fl   -> show fl
      Right dec -> show dec
    _ -> prettyFractional rat

hiFixedHex :: Word8 -> String
hiFixedHex num = (if num < 16 then  "0" else "") ++ showHex num ""

hiPrettyApply :: String -> Doc AnsiStyle -> Doc AnsiStyle
hiPrettyApply name insides = pretty name <> pretty "(" <> insides <> pretty ")"

hiPrettyString :: String -> Doc AnsiStyle
hiPrettyString s = hiPretty $ HiValueString $ T.pack s

hiPrettyPair :: (HiValue, HiValue) -> Doc AnsiStyle
hiPrettyPair (a, b) = hiPretty a <> pretty ": " <> hiPretty b

hiPrettyAction :: HiAction -> Doc AnsiStyle
hiPrettyAction (HiActionRead p) = hiPrettyApply "read" $ hiPrettyString p
hiPrettyAction (HiActionWrite p bs) = hiPrettyApply "write" $ hsep $ punctuate comma [hiPrettyString p, hiPretty $ HiValueBytes bs]
hiPrettyAction (HiActionMkDir p) = hiPrettyApply "mkdir" $ hiPrettyString p
hiPrettyAction (HiActionChDir p) = hiPrettyApply "cd" $ hiPrettyString p
hiPrettyAction HiActionCwd = pretty "cwd"
hiPrettyAction HiActionNow = pretty "now"
hiPrettyAction (HiActionRand a b) = hiPrettyApply "rand" $ pretty a <> pretty ", " <> pretty b
hiPrettyAction (HiActionEcho t) = hiPrettyApply "echo" $ hiPrettyString $ T.unpack t

hiPretty :: HiValue -> Doc AnsiStyle
hiPretty (HiValueFunction func) = pretty $ show func
hiPretty (HiValueNumber num) = pretty $ prettyRational num
hiPretty (HiValueString text) = enclose (pretty '\"') (pretty '\"') (pretty text)
hiPretty (HiValueList l) = encloseSep (pretty "[ ") (pretty " ]") (pretty ", ") (hiPretty <$> toList l)
hiPretty (HiValueBool b) = pretty $ toLower <$> show b
hiPretty (HiValueBytes bs) = encloseSep (pretty "[# ") (pretty " #]") (pretty " ") (pretty . hiFixedHex <$> B.unpack bs)
hiPretty (HiValueAction action) = hiPrettyAction action
hiPretty (HiValueTime t) = hiPrettyApply "parse-time" $ hiPrettyString $ show t
hiPretty (HiValueDict m) = encloseSep (pretty "{ ") (pretty " }") (pretty ", ") (hiPrettyPair <$> assocs m)
hiPretty HiValueNull = pretty "null"

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = annotate (color Green) . hiPretty

errMes:: String -> Doc AnsiStyle
errMes = annotate (color Red) . pretty
