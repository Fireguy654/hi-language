{-# LANGUAGE LambdaCase          #-}
module Main (main) where

import           Control.Exception             (displayException)
import           Control.Monad.Trans           (lift)
import qualified Data.Set                      as S
import           HW5.Action                    (HIO (runHIO),
                                                HiPermission (AllowRead, AllowTime, AllowWrite))
import           HW5.Base                      (HiExpr)
import           HW5.Evaluator                 (eval)
import           HW5.Parser                    (parse)
import           HW5.Pretty                    (errMes, prettyValue)
import           Prettyprinter                 (Doc, pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle, putDoc)
import           System.Console.Haskeline      (InputT, defaultSettings,
                                                getInputLine, outputStrLn,
                                                runInputT)

type Prompt = InputT IO

printDoc :: Doc AnsiStyle -> Prompt ()
printDoc doc = lift (putDoc doc) >> outputStrLn ""

evalAndPrint :: HiExpr -> Prompt ()
evalAndPrint expr = do
    eitherVal <- lift $ runHIO (eval expr) (S.fromList [AllowWrite, AllowRead, AllowTime])
    case eitherVal of
        Left err  -> printDoc $ errMes "EVALUATING ERROR: " <> pretty (show err)
        Right val -> printDoc $ prettyValue val

proccessInput :: String -> Prompt ()
proccessInput input = case parse input of
    Left err   -> printDoc $ errMes ("PARSING ERROR: " <> displayException err)
    Right expr -> evalAndPrint expr

commandLoop :: Prompt ()
commandLoop = getInputLine "hi> " >>= \case
    Nothing -> return ()
    Just input -> proccessInput input >> commandLoop

main :: IO ()
main = runInputT defaultSettings commandLoop
