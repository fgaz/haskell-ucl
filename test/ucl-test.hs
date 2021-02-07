{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.UCL
import Data.Map (fromList)
import Control.Monad (unless)
import System.Exit (exitFailure)

-- TODO use quickcheck when printing is implemented
main :: IO ()
main = do
  parsed1 <- parseString "0: 1min"
  print parsed1
  parsed <- parseString "\"a\": [12,34], 1:2, 1:3, 2:\"ab🌅c\", 3: yes, \"a\": [56]"
  print parsed
  unless (parsed == Right (UCLMap (fromList [(UCLText "1",UCLInt 2),(UCLText "2",UCLText "ab\127749c"),(UCLText "3",UCLBool True),(UCLText "a",UCLArray [UCLInt 12,UCLInt 34])])))
     exitFailure
