{-# LANGUAGE DeriveGeneric #-}

module DistrictFund where

import GHC.Generics

{-| A district's funds allocation to a bill. -}
data DistrictFund =
  DistrictFund { district :: String, bill :: String, amount :: Int }
  deriving (Generic)

