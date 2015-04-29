module Graphics.Formats.Assimp.Utils (
    logLn
  , logPrint
  ) where

{-# INLINE logging #-}
logging :: Bool
logging = False
 
{-# INLINE logLn #-}
logLn :: String -> IO ()
logLn s = if logging then putStrLn s else return ()

{-# INLINE logPrint #-}
logPrint :: Show a => a -> IO ()
logPrint = logLn . show
