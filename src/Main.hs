{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

newtype Id a = Id Int deriving (Generic, Show)
type Fund = Int

data Category =
  Category { categoryId :: Id Category, categoryName :: String }
  deriving (Generic, Show)

data Bill =
  Bill
    { billId :: Id Bill,
      billName :: String,
      billCategory :: Id Category,
      billAmount :: Fund } deriving (Generic, Show)

data District =
  District
    { districtId :: Id District,
      districtName :: String,
      districtAvailFund :: Fund } deriving (Generic, Show)

data CategoryFund =
  Cf
    { cfDistrict :: Id District,
      cfCategory :: Id Category,
      cfAmount :: Fund } deriving (Generic, Show)

data BillSpecificFund =
  Bsf
    { bsfDistrict :: Id District,
      bsfBill :: Id Bill,
      bsfAmount :: Fund } deriving (Generic, Show)

data Input =
  Input
    { inputCategories :: [Category],
      inputBills :: [Bill],
      inputDistricts :: [District],
      inputCdfs :: [CategoryFund],
      inputBsfs :: [BillSpecificFund],
      inputCategoryCaps :: [CategoryFund] } deriving (Generic, Show)

newtype Output =
  Output { output :: [BillSpecificFund] } deriving (Generic)

allocate _ = ()

instance FromJSON (Id a)
instance FromJSON BillSpecificFund
instance FromJSON CategoryFund
instance FromJSON District
instance FromJSON Bill
instance FromJSON Category
instance FromJSON Input

main :: IO ()
main = interact $ \input ->
  case decode (BS.pack input) :: Maybe Input of
    Just input -> BS.unpack . encode . allocate $ input
    Nothing -> BS.unpack . encode $ "Error: could not understand input"

