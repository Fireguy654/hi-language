{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module HW5.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  , checkPerm
  , liftIO
  ) where
import           Control.Exception      (Exception, throwIO)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadIO (liftIO))
import qualified Data.ByteString        as B
import qualified Data.ByteString.UTF8   as BU
import           Data.Sequence          (Seq (Empty, (:<|)))
import           Data.Set               (Set, member)
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime)
import           HW5.Base               (HiAction (HiActionChDir, HiActionCwd, HiActionEcho, HiActionMkDir, HiActionNow, HiActionRand, HiActionRead, HiActionWrite),
                                         HiMonad (runAction),
                                         HiValue (HiValueBytes, HiValueList, HiValueNull, HiValueNumber, HiValueString, HiValueTime))
import qualified System.Directory       as D
import           System.Random.Stateful (UniformRange (uniformRM), globalStdGen)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap func a = HIO $ fmap func . runHIO a

instance Applicative HIO where
  pure = HIO . const . pure
  (<*>) func a = HIO $ \perms -> runHIO func perms <*> runHIO a perms

instance Monad HIO where
  (>>=) a func = HIO $ \params -> runHIO a params >>= flip runHIO params . func

instance MonadIO HIO where
  liftIO = HIO . const

checkPerm :: HiPermission -> HIO ()
checkPerm perm = HIO $ \perms -> if member perm perms then return () else throwIO $ PermissionRequired perm

readAction :: IO a -> (a -> HiValue) -> HIO HiValue
readAction act func = do
  checkPerm AllowRead
  val <- liftIO act
  return $ func val

writeAction :: IO a -> HIO HiValue
writeAction act = do
  checkPerm AllowWrite
  _ <- liftIO act
  return HiValueNull

instance HiMonad HIO where
  runAction (HiActionRead p) = do
    checkPerm AllowRead
    isDir <- liftIO $ D.doesDirectoryExist p
    if isDir
      then do
        dirs <- liftIO $ D.listDirectory p
        return $ HiValueList $ foldr (\a acc -> HiValueString (T.pack a) :<| acc) Empty dirs
      else do
        bs <- liftIO $ B.readFile p
        if B.isValidUtf8 bs then return $ HiValueString $ T.pack $ BU.toString bs else return $ HiValueBytes bs
  runAction (HiActionWrite p bs) = writeAction $ B.writeFile p bs
  runAction (HiActionMkDir p) = writeAction $ D.createDirectoryIfMissing True p
  runAction (HiActionChDir p) = readAction (D.setCurrentDirectory p) (const HiValueNull)
  runAction HiActionCwd = readAction D.getCurrentDirectory (HiValueString . T.pack)
  runAction HiActionNow = do
    checkPerm AllowTime
    HiValueTime <$> liftIO getCurrentTime
  runAction (HiActionRand from to) = HiValueNumber . toRational <$> liftIO (uniformRM (from, to) globalStdGen)
  runAction (HiActionEcho t) = writeAction $ putStrLn $ T.unpack t
