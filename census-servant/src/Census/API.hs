{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Census.API where

import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

import           Control.Exception.Safe (throw)
import qualified Data.Aeson             as A
--import           Data.ByteString.Char8  (pack)
import qualified Data.Foldable          as F
--import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
--import           Data.Scientific        (Scientific)
import           Data.Text              (Text, unpack)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V

baseUrl = BaseUrl Https "api.census.gov" 443 "data"

data Census_Routes route = Census_Routes
  {
    _ACS :: route :- Capture "Year" Year :> "acs" :> Capture "span" Text :> QueryParams "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "key" Text :> Get '[JSON] A.Value
  , _SAIPE :: route :- "timeseries" :> "poverty" :> "saipe" :>  QueryParams "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "time" Year :> QueryParam "key" Text :> Get '[JSON] A.Value
  }
  deriving (Generic)

censusClients :: Census_Routes (AsClientT ClientM)
censusClients = genericClient

-- Census specific constants

type ApiKey = Text
censusApiKey :: ApiKey
censusApiKey = "2a9aeb4cae45db2d5e7ce1c9d0622caf68e8de15"


-- https://api.census.gov/data/2017/acs/acs5/variables
--data ACS_DataCode = C27018_002E
type Year = Int
type ACS_DataCode = Text

-- Want constructors that take smarter types, like a state/county or state/congressional district or whatever
data GeoCode where
  GeoCodeRawFor :: Text -> GeoCode
  GeoCodeRawForIn :: Text -> Text -> GeoCode
  AllStatesAndCounties :: GeoCode

geoCodeToQuery :: GeoCode -> (Maybe Text, Maybe Text)
geoCodeToQuery (GeoCodeRawFor forText) = (Just forText, Nothing)
geoCodeToQuery (GeoCodeRawForIn forText inText) = (Just forText, Just inText)
geoCodeToQuery AllStatesAndCounties = (Just "county:*", Just "state:*")

data ACS_Span = ACS1 | ACS3 | ACS5 deriving (Show,Enum,Eq,Ord,Bounded)
acsSpanToText :: ACS_Span -> Text
acsSpanToText ACS1 = "acs1"
acsSpanToText ACS3 = "acs3"
acsSpanToText ACS5 = "acs5"

getACSData :: Year -> ACS_Span -> [ACS_DataCode] -> GeoCode -> ClientM A.Value
getACSData year span codes geoCode =
  let (forM, inM) = geoCodeToQuery geoCode
  in (_ACS censusClients) year (acsSpanToText span) codes forM inM (Just censusApiKey)

data SAIPEDataCode = MedianHouseholdIncome |
                     MedianHouseholdIncomeMOE |
                     Poverty0to17Rate |
                     Poverty0to17RateMOE |
                     Poverty0to4Rate |
                     Poverty0to4RateMOE |
                     PovertyRate |
                     PovertyRateMOE

saipeDataCodeToText MedianHouseholdIncome    = "SAEMHI_PT"
saipeDataCodeToText MedianHouseholdIncomeMOE = "SAEMHI_MOE"
saipeDataCodeToText Poverty0to17Rate         = "SAEPOVRTO_17_PT"
saipeDataCodeToText Poverty0to17RateMOE      = "SAEPOVRTO_17_MOE"
saipeDataCodeToText Poverty0to4Rate          = "SAEPOVRTO_4_PT"
saipeDataCodeToText Poverty0to4RateMOE       = "SAEPOVRTO_4_MOE"
saipeDataCodeToText PovertyRate              = "SAEPOVTRTALL_PT"
saipeDataCodeToText PovertyRateMOE           = "SAEPOVTRTALL_MOE"

getSAIPEData :: Year -> GeoCode -> [SAIPEDataCode] -> ClientM A.Value
getSAIPEData year geo vars =
  let (forM, inM) = geoCodeToQuery geo
  in (_SAIPE censusClients) (saipeDataCodeToText <$> vars) forM inM (Just year) (Just censusApiKey)



{-
censusMaxPerPage :: Int
censusMaxPerPage = 100

data QueryLimit = QueryLimit { maxQueries :: Int, perTime :: C.DiffTime }
censusQueryLimit = QueryLimit 120 (C.secondsToDiffTime 35)

delayQueries :: QueryLimit -> IO ()
delayQueries (QueryLimit n per) =
  let sleepMicros = 1 + C.diffTimeToPicoseconds per `div` (fromIntegral $ 1000000 * n)
  in threadDelay (fromIntegral sleepMicros)
-}
