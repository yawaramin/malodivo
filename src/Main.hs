{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

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
  CategoryFund
    { cfDistrict :: Id District,
      cfCategory :: Id Category,
      cfAmount :: Fund } deriving (Generic, Show)

data BillFund =
  BillFund
    { bfDistrict :: Id District,
      bfBill :: Id Bill,
      bfAmount :: Fund } deriving (Generic, Show)

data Input =
  Input
    { inputCategories :: [Category],
      inputBills :: [Bill],
      inputDistricts :: [District],
      inputCdfs :: [CategoryFund], -- ^ Category default funds.
      inputBsfs :: [BillFund], -- ^ Bill-specific funds.
      inputCategoryCaps :: [CategoryFund] } deriving (Generic, Show)

newtype Output = Output { output :: [BillFund] } deriving (Generic)

districtFund categories bills cdfs bsfs categoryCaps (District { districtId }) =
  BillFund { bfDistrict = districtId, bfBill = Id 1, bfAmount = 1 }

allocate :: Input -> Output
allocate Input { inputCategories, inputBills, inputDistricts, inputCdfs, inputBsfs, inputCategoryCaps } =
  Output
    { output =
        map
          (districtFund inputCategories inputBills inputCdfs inputBsfs inputCategoryCaps)
          inputDistricts }

instance FromJSON (Id a)
instance ToJSON (Id a)
instance FromJSON BillFund
instance ToJSON BillFund
instance FromJSON CategoryFund
instance FromJSON District
instance FromJSON Bill
instance FromJSON Category
instance FromJSON Input
instance ToJSON Output

main :: IO ()
main = interact $ \input ->
  case decode (BS.pack input) :: Maybe Input of
    Just input -> BS.unpack . encode . allocate $ input
    Nothing -> BS.unpack . encode $ "Error: could not understand input"

