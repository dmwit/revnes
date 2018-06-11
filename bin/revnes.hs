module Main where

import Brick.Main
import Data.Default
import RevNES.UI

import Control.Monad.Except
import RevNES.MemMap.INES
import qualified Data.Map as M

main :: IO UI
main = do
	Right (_, chunks, memMap) <- runExceptT (autodetectLoad "dr_mario.zip")
	let ui = def
	    	{ memMapSources = chunks
	    	, memMaps = M.singleton "default" memMap
	    	, cpuDisplay = def { regions = byteRegions ui }
	    	}
	defaultMain app ui
