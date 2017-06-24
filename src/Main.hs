{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

import qualified Bill as B
import qualified Cap as C

instance FromJSON B.Bill
instance FromJSON C.Cap

data District =
  District
    { name :: String,
      availableFunds :: Int,
      categoryDefaultFunding :: [C.Cap],
      billSpecificFunding :: [BillSpecificFunding],
      caps :: [C.Cap] }
  deriving (Generic, Show)

instance FromJSON District

data BillSpecificFunding =
  BillSpecificFunding { bill :: String, amount :: Int }
  deriving (Generic, Show)

instance FromJSON BillSpecificFunding

data Input =
  Input { bills :: [B.Bill], districts :: [District] }
  deriving (Generic, Show)

instance FromJSON Input

data Output = Output () deriving (Generic)

instance ToJSON Output

allocate :: Input -> Output
allocate _ = Output ()

main :: IO ()
main = interact $ \input ->
  case decode (BS.pack input) :: Maybe Input of
    Just input -> BS.unpack . encode . allocate $ input
    Nothing -> ""
