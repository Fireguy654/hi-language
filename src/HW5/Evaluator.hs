{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}
module HW5.Evaluator
  ( eval
  ) where

import           Codec.Compression.Zlib     (CompressParams (compressLevel),
                                             bestCompression, compressWith,
                                             decompress, defaultCompressParams)
import           Codec.Serialise            (deserialise, serialise)
import           Control.Applicative        ((<|>))
import           Control.Monad.Except       (ExceptT, runExceptT)
import           Control.Monad.State        (MonadState (get, put), StateT,
                                             evalStateT)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString            as B
import qualified Data.ByteString.UTF8       as BU
import           Data.Foldable              (msum, toList)
import qualified Data.Map                   as M
import           Data.Ratio                 (denominator, numerator)
import           Data.Semigroup             (Semigroup (stimes))
import           Data.Sequence              (Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence              as S
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime, addUTCTime, diffUTCTime)
import           Data.Word                  (Word8)
import           HW5.Base                   (HiAction (HiActionChDir, HiActionEcho, HiActionMkDir, HiActionRand, HiActionRead, HiActionWrite),
                                             HiError (HiErrorArityMismatch, HiErrorDivideByZero, HiErrorInvalidArgument, HiErrorInvalidFunction),
                                             HiExpr (HiExprApply, HiExprDict, HiExprRun, HiExprValue),
                                             HiFun (..), HiMonad,
                                             HiValue (HiValueAction, HiValueBool, HiValueBytes, HiValueDict, HiValueFunction, HiValueList, HiValueNull, HiValueNumber, HiValueString, HiValueTime),
                                             runAction)
import           HW5.Primitive              (checkTrue, countMap, elemByInd,
                                             eqBoolRat, greaterBoolRat,
                                             lessBoolRat, notEqBoolRat,
                                             notGreaterBoolRat, notLessBoolRat,
                                             seqRange, unpackBytes)
import           Text.Read                  (readMaybe)

type Evaluator m a = ExceptT HiError (StateT [HiExpr] m) a

type Getter m a = HiValue -> Evaluator m a

type Construct a = a -> HiValue

eUnAsk :: HiMonad m => Evaluator m HiExpr
eUnAsk = get >>= \case
  [expr] -> return expr
  _ -> throwE HiErrorArityMismatch

eBinAsk :: HiMonad m => Evaluator m (HiExpr, HiExpr)
eBinAsk = get >>= \case
  [exprA, exprB] -> return (exprA, exprB)
  _ -> throwE HiErrorArityMismatch

eTerAsk :: HiMonad m => Evaluator m (HiExpr, HiExpr, HiExpr)
eTerAsk = get >>= \case
  [exprA, exprB, exprC] -> return (exprA, exprB, exprC)
  _ -> throwE HiErrorArityMismatch

eCache :: HiMonad m => [HiValue] -> Evaluator m ()
eCache res = put $ HiExprValue <$> res

eCollectPair :: HiMonad m => (HiExpr, HiExpr) -> Evaluator m (HiValue, HiValue)
eCollectPair (a, b) = do
    aVal <- eExpr a
    bVal <- eExpr b
    return (aVal, bVal)

eGetRational :: HiMonad m => HiValue -> Evaluator m Rational
eGetRational (HiValueNumber num) = return num
eGetRational _                   = throwE HiErrorInvalidArgument

eGetBool :: HiMonad m => HiValue -> Evaluator m Bool
eGetBool (HiValueBool b) = return b
eGetBool _               = throwE HiErrorInvalidArgument

eGetText :: HiMonad m => HiValue -> Evaluator m T.Text
eGetText (HiValueString text) = return text
eGetText _                    = throwE HiErrorInvalidArgument

eGetSeq :: HiMonad m => HiValue -> Evaluator m (Seq HiValue)
eGetSeq (HiValueList l) = return l
eGetSeq _               = throwE HiErrorInvalidArgument

eGetBytes :: HiMonad m => HiValue -> Evaluator m B.ByteString
eGetBytes (HiValueBytes bs) = return bs
eGetBytes _                 = throwE HiErrorInvalidArgument

eGetTime :: HiMonad m => HiValue -> Evaluator m UTCTime
eGetTime (HiValueTime t) = return t
eGetTime _               = throwE HiErrorInvalidArgument

eGetFunValue :: HiMonad m => HiValue -> Evaluator m HiFun
eGetFunValue (HiValueFunction func) = return func
eGetFunValue _                      = throwE HiErrorInvalidArgument

eGetActionValue :: HiMonad m => HiValue -> Evaluator m HiAction
eGetActionValue (HiValueAction act) = return act
eGetActionValue _                   = throwE HiErrorInvalidArgument

eGetDivisor :: HiMonad m => HiValue -> Evaluator m Rational
eGetDivisor val = do
  rat <- eGetRational val
  if rat == 0 then throwE HiErrorDivideByZero else return rat

eGetInt :: HiMonad m => HiValue -> Evaluator m Int
eGetInt val = do
  rat <- eGetRational val
  let (q, r) = quotRem (numerator rat) (denominator rat)
  if r == 0 then return $ fromInteger q else throwE HiErrorInvalidArgument

eGetString :: HiMonad m => HiValue -> Evaluator m String
eGetString val = T.unpack <$> eGetText val

eGetMap :: HiMonad m => HiValue -> Evaluator m (M.Map HiValue HiValue)
eGetMap (HiValueDict m) = return m
eGetMap _               = throwE HiErrorInvalidArgument

sliceByInd :: HiMonad m => Int -> (Int -> Int -> HiValue) -> HiValue -> HiValue -> Evaluator m HiValue
sliceByInd len subFunc HiValueNull val = sliceByInd len subFunc (HiValueNumber 0) val
sliceByInd len subFunc val HiValueNull = sliceByInd len subFunc val $ HiValueNumber $ toRational len
sliceByInd len subFunc aVal bVal = do
  aInd <- eGetInt aVal
  bInd <- eGetInt bVal
  let l = if aInd < 0 then aInd + len else aInd
      r = if bInd < 0 then bInd + len else bInd
  return $ subFunc (max l 0) (max r 0)

foldByFun :: HiMonad m => HiFun -> Seq HiValue -> Evaluator m HiValue
foldByFun _ Empty = throwE HiErrorInvalidArgument
foldByFun _ (a :<| Empty) = return a
foldByFun _ (Empty :|> a) = return a
foldByFun fun l = eExpr $ foldl1 (\acc a -> HiExprApply (HiExprValue $ HiValueFunction fun) [acc, a]) (HiExprValue <$> l)

valToBytes :: HiMonad m => HiValue -> Evaluator m Word8
valToBytes val@(HiValueNumber num) | num >= toRational (minBound :: Word8) && num <= toRational (maxBound :: Word8) = fromIntegral <$> eGetInt val
valToBytes _ = throwE HiErrorInvalidArgument

stringToTime :: HiMonad m => HiValue -> Evaluator m HiValue
stringToTime val = eGetString val >>= (\case
  (Just t) -> return $ HiValueTime t
  Nothing -> return HiValueNull) . (readMaybe @UTCTime)

eGetSimpleMul :: HiMonad m => (b -> a -> b) -> b -> Getter m a -> Construct b -> Evaluator m HiValue
eGetSimpleMul func def getter constructor = do
  elems <- get
  elemVals <- traverse eExpr elems
  eCache elemVals
  elemsA <- traverse getter elemVals
  return $ constructor $ foldl func def elemsA

eGetEvalUn :: HiMonad m => (a -> Evaluator m b) -> Getter m a -> Evaluator m b
eGetEvalUn func getter = do
  expr <- eUnAsk
  val <- eExpr expr
  eCache [val]
  arg <- getter val
  func arg

eGetSimpleUn :: HiMonad m => (a -> b) -> Getter m a -> Construct b -> Evaluator m HiValue
eGetSimpleUn func getter constructor = eGetEvalUn (return . constructor . func) getter

eGetEvalBin :: HiMonad m => (a -> b -> Evaluator m c) -> Getter m a -> Getter m b -> Evaluator m c
eGetEvalBin func getterA getterB = do
  (exprA, exprB) <- eBinAsk
  aVal <- eExpr exprA
  put [HiExprValue aVal, exprB]
  bVal <- eExpr exprB
  eCache [aVal, bVal]
  a <- getterA aVal
  b <- getterB bVal
  func a b

eGetCompBin :: HiMonad m => (a -> b -> c) -> Getter m a -> Getter m b -> Construct c -> Evaluator m HiValue
eGetCompBin func getterA getterB constructor = eGetEvalBin (\a b -> return $ constructor $ func a b) getterA getterB

eGetCommutativeBin :: HiMonad m => (a -> b -> c) -> Getter m a -> Getter m b -> Construct c -> Evaluator m HiValue
eGetCommutativeBin func getterFir getterSec constructor =
      eGetCompBin func getterFir getterSec constructor
  <|> eGetCompBin (flip func) getterSec getterFir constructor

eGetSimpleBin :: HiMonad m => (a -> a -> b) -> Getter m a -> Construct b -> Evaluator m HiValue
eGetSimpleBin func getter = eGetCompBin func getter getter

eGetHiValueBin :: HiMonad m => (HiValue -> HiValue -> Evaluator m a) -> Evaluator m a
eGetHiValueBin func = eGetEvalBin func return return

eGetBinRational :: HiMonad m => (Rational -> Rational -> Rational) -> Evaluator m HiValue
eGetBinRational func = eGetSimpleBin func eGetRational HiValueNumber

eGetSimpleComparison :: HiMonad m => (a -> a -> Bool) -> Getter m a -> Evaluator m HiValue
eGetSimpleComparison func getter = eGetSimpleBin func getter HiValueBool

eGetComparison :: HiMonad m => (forall a. Ord a => a -> a -> Bool) -> (HiValue -> HiValue -> Evaluator m Bool) -> Evaluator m HiValue
eGetComparison func compFunc = msum
  [ eGetSimpleComparison func eGetRational
  , eGetSimpleComparison func eGetBool
  , eGetSimpleComparison func eGetFunValue
  , eGetSimpleComparison func eGetText
  , HiValueBool <$> eGetHiValueBin compFunc ]

eGetLazyIf :: HiMonad m => Evaluator m HiValue
eGetLazyIf = do
  (exprCond, exprA, exprB) <- eTerAsk
  condVal <- eExpr exprCond
  put [HiExprValue condVal, exprA, exprB]
  cond <- eGetBool condVal
  if cond then eExpr exprA else eExpr exprB

eGetLazyBool :: HiMonad m => (HiValue -> Bool) -> Evaluator m HiValue
eGetLazyBool check = do
  (exprA, exprB) <- eBinAsk
  a <- eExpr exprA
  put [HiExprValue a, exprB]
  if check a
    then return a
    else eExpr exprB

eGetUnString :: HiMonad m => (T.Text -> a) -> Construct a -> Evaluator m HiValue
eGetUnString func = eGetSimpleUn func eGetText

eGetStringOp :: HiMonad m => (T.Text -> T.Text) -> Evaluator m HiValue
eGetStringOp func = eGetUnString func HiValueString

eGetBinString :: HiMonad m => (T.Text -> T.Text -> T.Text) -> Evaluator m HiValue
eGetBinString func = eGetSimpleBin func eGetText HiValueString

eGetUnByte :: HiMonad m => (B.ByteString -> a) -> Construct a -> Evaluator m HiValue
eGetUnByte func = eGetSimpleUn func eGetBytes

eGetByteOp :: HiMonad m => (B.ByteString -> B.ByteString) -> Evaluator m HiValue
eGetByteOp func = eGetUnByte func HiValueBytes

eGetUnAction :: HiMonad m => (T.Text -> HiValue) -> Evaluator m HiValue
eGetUnAction fun = eGetSimpleUn fun eGetText id

eGetUnStringAction :: HiMonad m => (String -> HiValue) -> Evaluator m HiValue
eGetUnStringAction func = eGetUnAction $ func . T.unpack

eGetUnMap :: HiMonad m => (M.Map HiValue HiValue -> a) -> Construct a -> Evaluator m HiValue
eGetUnMap func = eGetSimpleUn func eGetMap

eGetMapConv :: HiMonad m => (a -> M.Map HiValue HiValue) -> Getter m a -> Evaluator m HiValue
eGetMapConv func getter = eGetSimpleUn func getter HiValueDict

eGetAdd :: HiMonad m => Evaluator m HiValue
eGetAdd = msum
  [ eGetBinRational (+)
  , eGetBinString (<>)
  , eGetSimpleBin (<>) eGetSeq HiValueList
  , eGetSimpleBin (<>) eGetBytes HiValueBytes
  , eGetCommutativeBin (addUTCTime . fromRational) eGetRational eGetTime HiValueTime ]

eGetSub :: HiMonad m => Evaluator m HiValue
eGetSub = msum
  [ eGetBinRational (-)
  , eGetSimpleBin diffUTCTime eGetTime (HiValueNumber . toRational) ]

eGetMult :: HiMonad m => Evaluator m HiValue
eGetMult = msum
  [ eGetBinRational (*)
  , eGetCommutativeBin stimes eGetInt eGetText HiValueString
  , eGetCommutativeBin stimes eGetInt eGetSeq HiValueList
  , eGetCommutativeBin stimes eGetInt eGetBytes HiValueBytes ]

eGetDiv :: HiMonad m => Evaluator m HiValue
eGetDiv = msum
  [ eGetCompBin (/) eGetRational eGetDivisor HiValueNumber
  , eGetBinString (\a b -> a <> T.pack "/" <> b) ]

eGetIndexing :: HiMonad m => Int -> (Int -> HiValue) -> (Int -> Int -> HiValue) -> Evaluator m HiValue
eGetIndexing len getFunc subFun = msum
  [ eGetSimpleUn (elemByInd len getFunc) eGetInt id
  , eGetHiValueBin $ sliceByInd len subFun ]

eGetLength :: HiMonad m => Evaluator m HiValue
eGetLength = let constructor = (HiValueNumber . toRational) in msum
  [ eGetUnString T.length constructor
  , eGetSimpleUn S.length eGetSeq constructor ]

eGetReverse :: HiMonad m => Evaluator m HiValue
eGetReverse = msum
  [ eGetStringOp T.reverse
  , eGetSimpleUn S.reverse eGetSeq HiValueList ]

eGetCount :: HiMonad m => Evaluator m HiValue
eGetCount = msum
  [ eGetMapConv (countMap (HiValueString . T.singleton)) eGetString
  , eGetMapConv (countMap id . unpackBytes) eGetBytes
  , eGetMapConv (countMap id) eGetSeq ]

eGetHiFun :: HiMonad m => HiFun -> Evaluator m HiValue
eGetHiFun HiFunAdd = eGetAdd
eGetHiFun HiFunSub = eGetSub
eGetHiFun HiFunMul = eGetMult
eGetHiFun HiFunDiv = eGetDiv
eGetHiFun HiFunNot = eGetSimpleUn not eGetBool HiValueBool
eGetHiFun HiFunAnd = eGetLazyBool $ not . checkTrue
eGetHiFun HiFunOr = eGetLazyBool checkTrue
eGetHiFun HiFunLessThan = eGetComparison (<) lessBoolRat
eGetHiFun HiFunGreaterThan = eGetComparison (>) greaterBoolRat
eGetHiFun HiFunEquals = eGetComparison (==) eqBoolRat
eGetHiFun HiFunNotLessThan = eGetComparison (>=) notLessBoolRat
eGetHiFun HiFunNotGreaterThan = eGetComparison (<=) notGreaterBoolRat
eGetHiFun HiFunNotEquals = eGetComparison (/=) notEqBoolRat
eGetHiFun HiFunIf = eGetLazyIf
eGetHiFun HiFunLength = eGetLength
eGetHiFun HiFunToUpper = eGetStringOp T.toUpper
eGetHiFun HiFunToLower = eGetStringOp T.toLower
eGetHiFun HiFunReverse = eGetReverse
eGetHiFun HiFunTrim = eGetStringOp T.strip
eGetHiFun HiFunList = eGetSimpleMul (:|>) Empty return HiValueList
eGetHiFun HiFunRange = eGetSimpleBin seqRange eGetRational HiValueList
eGetHiFun HiFunFold = eGetEvalBin foldByFun eGetFunValue eGetSeq
eGetHiFun HiFunPackBytes = eGetEvalUn (fmap (HiValueBytes . B.pack . toList) . traverse valToBytes) eGetSeq
eGetHiFun HiFunUnpackBytes = eGetUnByte unpackBytes HiValueList
eGetHiFun HiFunEncodeUtf8 = eGetUnString (BU.fromString . T.unpack) HiValueBytes
eGetHiFun HiFunDecodeUtf8 = eGetUnByte (\bs -> if B.isValidUtf8 bs then HiValueString $ T.pack $ BU.toString bs else HiValueNull) id
eGetHiFun HiFunZip = eGetByteOp $ B.toStrict . compressWith (defaultCompressParams { compressLevel = bestCompression }) . B.fromStrict
eGetHiFun HiFunUnzip = eGetByteOp $ B.toStrict . decompress . B.fromStrict
eGetHiFun HiFunSerialise = eGetSimpleUn serialise return (HiValueBytes . B.toStrict)
eGetHiFun HiFunDeserialise = eGetUnByte (deserialise . B.fromStrict) id
eGetHiFun HiFunRead = eGetUnStringAction $ HiValueAction . HiActionRead
eGetHiFun HiFunWrite = eGetSimpleBin (\p inp -> HiActionWrite p (BU.fromString inp)) eGetString HiValueAction
eGetHiFun HiFunMkDir = eGetUnStringAction $ HiValueAction . HiActionMkDir
eGetHiFun HiFunChDir = eGetUnStringAction $ HiValueAction . HiActionChDir
eGetHiFun HiFunParseTime = eGetEvalUn stringToTime return
eGetHiFun HiFunRand = eGetSimpleBin HiActionRand eGetInt HiValueAction
eGetHiFun HiFunEcho = eGetUnAction $ HiValueAction . HiActionEcho
eGetHiFun HiFunCount = eGetCount
eGetHiFun HiFunKeys = eGetUnMap M.keys (HiValueList . S.fromList)
eGetHiFun HiFunValues = eGetUnMap M.elems (HiValueList . S.fromList)
eGetHiFun HiFunInvert = eGetUnMap (M.map HiValueList . M.foldrWithKey (\k a acc -> M.insertWith (<>) a (k :<| S.Empty) acc) M.empty) HiValueDict

eGetFun :: HiMonad m => HiValue -> Evaluator m HiValue
eGetFun (HiValueFunction func) = eGetHiFun func
eGetFun (HiValueString text) = eGetIndexing (T.length text) (HiValueString . T.singleton . T.index text) (\l r -> HiValueString $ T.drop l $ T.take r text)
eGetFun (HiValueList s) = eGetIndexing (S.length s) (S.index s) (\l r -> HiValueList $ S.drop l $ S.take r s)
eGetFun (HiValueBytes sb) = eGetIndexing (B.length sb) (HiValueNumber . toRational . B.index sb) (\l r -> HiValueBytes $ B.drop l $ B.take r sb)
eGetFun (HiValueDict m) = eGetSimpleUn (\val -> M.findWithDefault HiValueNull val m) return id
eGetFun _ = throwE HiErrorInvalidFunction

eExpr :: HiMonad m => HiExpr -> Evaluator m HiValue
eExpr (HiExprApply funcExpr exprs) = do
  funcVal <- eExpr funcExpr
  prevExprs <- get
  put exprs
  val <- eGetFun funcVal
  put prevExprs
  return val
eExpr (HiExprDict pairs) = HiValueDict . M.fromList <$> traverse eCollectPair pairs
eExpr (HiExprValue val) = return val
eExpr (HiExprRun expr) = do
  act <- eExpr expr >>= eGetActionValue
  lift $ lift $ runAction act

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = evalStateT (runExceptT (eExpr expr)) []
