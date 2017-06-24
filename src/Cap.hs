{-# LANGUAGE DeriveGeneric #-}

module Cap where

import GHC.Generics

data Cap =
  Cap { category :: String, amount :: Int } deriving (Generic, Show)

