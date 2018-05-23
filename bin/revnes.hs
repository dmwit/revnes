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
	defaultMain app def { memMapSources = chunks, memMaps = M.singleton "default" memMap }
