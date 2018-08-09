{-# LANGUAGE CPP #-}
module RevNES.Debug.Trace where

#ifdef DEBUG
import Control.DeepSeq
import Control.Exception
import Data.Time
import System.Posix.Process
#endif

import Control.Monad
import System.IO.Unsafe

traceFileName :: FilePath
traceFileName = "revnes_debug_output.txt"

traceIO :: String -> a -> IO a
traceIO msg val = do
#ifdef DEBUG
	-- avoid nesting calls to appendFile in case msg has some tracing going on inside it
	evaluate (rnf msg)
	now <- getCurrentTime
	pid <- getProcessID
	appendFile traceFileName (unwords [show now, show pid, msg] ++ "\n")
#endif
	return val

{-# NOINLINE trace #-}
trace :: String -> a -> a
trace msg val = unsafePerformIO (traceIO msg val)

traceIdJoin :: String -> String
traceIdJoin = join (trace . id)

traceShow :: Show a => a -> b -> b
traceShow = trace . show

traceShowJoin :: Show a => a -> a
traceShowJoin = join (trace . show)
