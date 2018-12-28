{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Census.API where

import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

import           Control.Exception.Safe (throw)
import           Control.Lens           ((^.), (^..), (^?))
import qualified Control.Lens           as L
import           Control.Monad          (forM_, join, mapM_, sequence)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson             as A
import qualified Data.Aeson.Lens        as L
import           Data.ByteString.Lazy   (fromStrict)
import qualified Data.Foldable          as F
import qualified Data.List              as List
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text, intercalate, unpack)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vec
import           Data.Vinyl             as V
import           Data.Vinyl.Functor     as V
import           Frames                 as F
import           Frames                 ((:->), (&:))
import           Frames.CSV             as F
import qualified Pipes                  as P

-- state FIPS datatypes
F.tableTypes "StateFIPSAndNames" "/Users/adam/DataScience/census-tools/conversion-data/states.csv"

stateFIPSAndNamesCSV = "conversion-data/states.csv"

-- state table is small so we can load it into memory.  No need to stream
getStateFIPSFrame :: IO (F.Frame StateFIPSAndNames)
getStateFIPSFrame = F.inCoreAoS $ F.readTable stateFIPSAndNamesCSV

baseUrl = BaseUrl Https "api.census.gov" 443 "data"

data Census_Routes route = Census_Routes
  {
    _ACS :: route :- Capture "Year" Year :> "acs" :> Capture "span" Text :> QueryParams "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "key" Text :> Get '[JSON] A.Value
  , _SAIPE :: route :- "timeseries" :> "poverty" :> "saipe" :>  QueryParam "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "time" Year :> QueryParam "key" Text :> Get '[JSON] A.Value
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
                     Poverty0to17Count |
                     Poverty0to17MOE |
                     Poverty0to4Rate |
                     Poverty0to4Count |
                     Poverty0to4MOE |
                     PovertyRate |
                     PovertyCount |
                     PovertyMOE

saipeDataCodeToText MedianHouseholdIncome    = "SAEMHI_PT"
saipeDataCodeToText MedianHouseholdIncomeMOE = "SAEMHI_MOE"
saipeDataCodeToText Poverty0to17Rate         = "SAEPOVRTO_17_PT"
saipeDataCodeToText Poverty0to17Count        = "SAEPOV0_17_PT"
saipeDataCodeToText Poverty0to17MOE          = "SAEPOV0_17_MOE"
saipeDataCodeToText Poverty0to4Rate          = "SAEPOVRT0_4_PT"
saipeDataCodeToText Poverty0to4Count         = "SAEPOVT0_4_PT"
saipeDataCodeToText Poverty0to4MOE           = "SAEPOV0_4_MOE"
saipeDataCodeToText PovertyRate              = "SAEPOVRTALL_PT"
saipeDataCodeToText PovertyCount             = "SAEPOVALL_PT"
saipeDataCodeToText PovertyMOE               = "SAEPOVTALL_MOE"

--type StateFIPS = "stateFIPS" :-> Int
declareColumn "CountyFIPS" ''Int -- = "countyFIPS" :-> Int
type MedianHI = "medianHI" :-> Int
type MedianHI_MOE = "medianHI_MOE" :-> Int
type PovertyR = "povertyR" :-> Double
type YearF = "year" :-> Int
type SAIPE = '[MedianHI, MedianHI_MOE, PovertyR, YearF, StateFIPS, CountyFIPS]

jsonTextArrayToList :: A.Value -> [Text]
jsonTextArrayToList x = x ^.. L.values . L._String

jsonArraysOfTextToPipe :: (MonadIO m, Monad m) => m A.Value -> P.Producer Text m ()
jsonArraysOfTextToPipe mv = do
  v <- P.lift mv
--  liftIO $ putStrLn $ show v
  let vs = v ^.. L.values -- get rid of header row
  forM_ vs $ \v -> do
    let csv = intercalate "," $ jsonTextArrayToList v
--    liftIO $ putStr $ show csv ++ ": "
    P.yield csv
  return ()

jsonArraysToRecordPipe :: (F.ReadRec rs, Monad m, MonadIO m) => m A.Value -> P.Producer (F.Rec (Either Text :. F.ElField) rs) m ()
jsonArraysToRecordPipe mv = jsonArraysOfTextToPipe mv P.>-> F.pipeTableEither

mapEither :: Monad m => P.Pipe (F.Rec (Either Text :. F.ElField) rs) (F.Record rs) m ()
mapEither = do
  eRec <- P.await
  case (F.rtraverse V.getCompose eRec) of
    Left _ -> mapEither -- skip it
    Right r -> P.yield r >> mapEither 

-- for debugging
eitherRecordPrint :: (V.RMap rs, V.ReifyConstraint Show V.ElField rs, V.RecordToList rs) => F.Rec (Either Text :. F.ElField) rs -> IO ()
eitherRecordPrint eRec =
  case (F.rtraverse V.getCompose eRec) of
    Left err -> liftIO $ putStrLn $ "Error: " ++ (unpack err)
    Right r  -> liftIO $ print r


getSAIPEData :: Year -> GeoCode -> [SAIPEDataCode] -> ClientM A.Value
getSAIPEData year geo vars =
  let (forM, inM) = geoCodeToQuery geo
  in (_SAIPE censusClients) (Just $ intercalate "," $ (saipeDataCodeToText <$> vars)) forM inM (Just year) (Just censusApiKey)


