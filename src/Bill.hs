{-# LANGUAGE DeriveGeneric #-}

module Bill where

import GHC.Generics

data Bill =
  Bill { name :: String, category :: String, amount :: Int }
  deriving (Generic, Show)

