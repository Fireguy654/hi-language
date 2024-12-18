module HW5.Primitive
  ( lessBoolRat
  , greaterBoolRat
  , eqBoolRat
  , notLessBoolRat
  , notGreaterBoolRat
  , notEqBoolRat
  , elemByInd
  , seqRange
  , unpackBytes
  , checkTrue
  , countMap
  ) where
import           Control.Monad.Except       (ExceptT)
import           Control.Monad.State        (StateT)
import           Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString            as B
import           Data.Foldable              (Foldable (foldr'))
import qualified Data.Map                   as M
import           Data.Sequence              (Seq (Empty, (:<|)))
import           HW5.Base                   (HiError (HiErrorInvalidArgument),
                                             HiExpr,
                                             HiValue (HiValueBool, HiValueNull, HiValueNumber))

type Evaluator m = ExceptT HiError (StateT [HiExpr] m)

lessBoolRat :: Monad m => HiValue -> HiValue -> Evaluator m Bool
lessBoolRat (HiValueNumber _) (HiValueBool _) = return False
lessBoolRat (HiValueBool _) (HiValueNumber _) = return True
lessBoolRat _ _                               = throwE HiErrorInvalidArgument

greaterBoolRat :: Monad m => HiValue -> HiValue -> Evaluator m Bool
greaterBoolRat a b = not <$> lessBoolRat a b

eqBoolRat :: Monad m => HiValue -> HiValue -> Evaluator m Bool
eqBoolRat (HiValueNumber _) (HiValueBool _) = return False
eqBoolRat (HiValueBool _) (HiValueNumber _) = return False
eqBoolRat _ _                               = throwE HiErrorInvalidArgument

notLessBoolRat :: Monad m => HiValue -> HiValue -> Evaluator m Bool
notLessBoolRat = greaterBoolRat

notGreaterBoolRat :: Monad m => HiValue -> HiValue -> Evaluator m Bool
notGreaterBoolRat = lessBoolRat

notEqBoolRat :: Monad m => HiValue -> HiValue -> Evaluator m Bool
notEqBoolRat a b = not <$> eqBoolRat a b

inBorder :: Int -> Int -> Bool
inBorder ind border = ind >= 0 && ind < border

elemByInd :: Int -> (Int -> HiValue) -> Int -> HiValue
elemByInd len getFunc ind = if inBorder ind len then getFunc ind else HiValueNull

seqRange :: Rational -> Rational -> Seq HiValue
seqRange from to | from <= to = HiValueNumber from :<| seqRange (from + 1) to
seqRange _ _ = Empty

unpackBytes :: B.ByteString -> Seq HiValue
unpackBytes bs = foldr' (\a acc -> HiValueNumber (toRational a) :<| acc) Empty (B.unpack bs)

checkTrue :: HiValue -> Bool
checkTrue (HiValueBool False) = False
checkTrue HiValueNull         = False
checkTrue _                   = True

countMap :: (Foldable t) => (a -> HiValue) -> t a ->  M.Map HiValue HiValue
countMap constructor t = M.map HiValueNumber $ foldr (\a acc -> let key = constructor a in M.insertWith (+) key 1 acc) M.empty t
