{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE PolyKinds                 #-}
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
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
module Census.API where

import qualified Census.Fields as CF 

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
import           Data.Vinyl.TypeLevel   as V
import           Frames                 as F
import           Frames                 ((:->), (&:))
import           Frames.CSV             as F
import           Frames.InCore          as FI
import qualified Pipes                  as P

stateFIPSAndNamesCSV = "conversion-data/states.csv"

-- state table is small so we can load it into memory.  No need to stream
getStateFIPSFrame :: IO (F.Frame CF.StateFIPSAndNames)
getStateFIPSFrame = F.inCoreAoS $ F.readTable stateFIPSAndNamesCSV

baseUrl = BaseUrl Https "api.census.gov" 443 "data"

data Census_Routes route = Census_Routes
  {
    _ACS :: route :- Capture "Year" CF.Year :> "acs" :> Capture "span" Text :> QueryParam "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "key" Text :> Get '[JSON] A.Value
  , _SAIPE :: route :- "timeseries" :> "poverty" :> "saipe" :>  QueryParam "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "time" CF.Year :> QueryParam "key" Text :> Get '[JSON] A.Value
  }
  deriving (Generic)

censusClients :: Census_Routes (AsClientT ClientM)
censusClients = genericClient

-- Census specific constants

type ApiKey = Text
censusApiKey :: ApiKey
censusApiKey = "2a9aeb4cae45db2d5e7ce1c9d0622caf68e8de15"


-- https://api.census.gov/data/2017/acs/acs5/variables
data ACS_Span = ACS1 | ACS3 | ACS5 deriving (Show,Enum,Eq,Ord,Bounded)
acsSpanToText :: ACS_Span -> Text
acsSpanToText ACS1 = "acs1"
acsSpanToText ACS3 = "acs3"
acsSpanToText ACS5 = "acs5"

getACSData :: CF.Year -> ACS_Span -> CF.GeoCode a -> [CF.ACS_DataCode] -> ClientM A.Value
getACSData year span geoCode codes  =
  let (forM, inM) = CF.geoCodeToQuery geoCode
      getQ = Just $ intercalate "," codes
  in (_ACS censusClients) year (acsSpanToText span) getQ forM inM (Just censusApiKey)


type ACSQueryFields fs gs = (CF.ACS_CodeList fs
                            , RMap (fs V.++ gs)
                            , V.ReifyConstraint Show V.ElField (fs V.++ gs)
                            , V.RecordToList (fs V.++ gs)
                            , FI.RecVec (fs V.++ gs)
                            , F.ReadRec (fs V.++ gs))

getACSDataFrame :: forall fs gs. ACSQueryFields fs gs
                => CF.Year -> ACS_Span -> CF.GeoCode gs -> ClientM (F.FrameRec (fs V.++ gs))
getACSDataFrame year span geoCode = do
  let codes = CF.acsCodeList @fs
  asAeson <- getACSData year span geoCode codes
  liftIO $ FI.inCoreAoS $ jsonArraysToRecordPipe (return asAeson) P.>-> mapEither
  
--data ACSDataCode = ACS_MedianHouseholdIncome

--acsDataCodeToText = "B19013"

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

mapEither :: (Monad m, MonadIO m, RMap rs, V.ReifyConstraint Show V.ElField rs, V.RecordToList rs) => P.Pipe (F.Rec (Either Text :. F.ElField) rs) (F.Record rs) m ()
mapEither = do
  eRec <- P.await
--  liftIO $ eitherRecordPrint eRec
  case (F.rtraverse V.getCompose eRec) of
    Left _ -> mapEither -- skip it
    Right r -> P.yield r >> mapEither 

-- for debugging
eitherRecordPrint :: (V.RMap rs, V.ReifyConstraint Show V.ElField rs, V.RecordToList rs) => F.Rec (Either Text :. F.ElField) rs -> IO ()
eitherRecordPrint eRec =
  case (F.rtraverse V.getCompose eRec) of
    Left err -> liftIO $ putStrLn $ "Error: " ++ (unpack err)
    Right r  -> liftIO $ print r

getSAIPEData :: CF.Year -> CF.GeoCode a -> [SAIPEDataCode] -> ClientM A.Value
getSAIPEData year geo vars =
  let (forM, inM) = CF.geoCodeToQuery geo
  in (_SAIPE censusClients) (Just $ intercalate "," $ (saipeDataCodeToText <$> vars)) forM inM (Just year) (Just censusApiKey)


