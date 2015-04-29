module Graphics.Formats.Assimp.Utils (
    logLn
  , logPrint
  ) where

logging :: Bool
logging = True

logLn :: String -> IO ()
logLn s = if logging then putStrLn s else return ()

logPrint :: Show a => a -> IO ()
logPrint = logLn . show
