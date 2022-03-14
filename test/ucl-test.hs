{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.UCL
import Data.Map (fromList)
import Control.Monad (unless)
import System.Exit (exitFailure)

-- TODO use quickcheck when printing is implemented
main :: IO ()
main = do
  success <- allM (uncurry test)
    [ ("0: 1min", UCLMap $ fromList [("0", UCLTime 60)])
    , ( "\"a\": [12,34], 1:2, 1:3, 2:\"ab🌅c\", 3: yes, \"a\": [56], \"b\": \"foo\""
      , UCLMap (fromList
          [ ("1",UCLInt 2)
          , ("2",UCLText "ab\127749c")
          , ("3",UCLBool True)
          , ("a",UCLArray [UCLInt 12, UCLInt 34, UCLInt 56])
          , ("b",UCLText "foo")]))
    -- TODO add UCLNull
    --, ("0: false, 1: null, 2: true", UCLMap $ fromList [("0", UCLBool False), ("1", UCLNull), ("2", UCLBool True)])
    ]
  unless success exitFailure

allM :: (Applicative m, Traversable f) => (a -> m Bool) -> f a -> m Bool
allM f xs = and <$> traverse f xs

-- TODO if this gets any bigger, use an actual test framework
test :: String -> UCL -> IO Bool
test str expected = do
  actual <- parseString str
  if actual == Right expected
  then pure True
  else do
    putStrLn "expected:"
    print expected
    putStrLn "actual:"
    print actual
    pure False
