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

import qualified Census.Fields                 as CF

import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

import           Control.Exception.Safe         ( throw )
import           Control.Lens                   ( (^.)
                                                , (^..)
                                                , (^?)
                                                )
import qualified Control.Lens                  as L
import           Control.Monad                  ( forM_
                                                , join
                                                , mapM_
                                                , sequence
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Lens               as L
import           Data.ByteString.Lazy           ( fromStrict )
import qualified Data.Foldable                 as F
import qualified Data.List                     as List
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , intercalate
                                                , unpack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vec
import           Data.Vinyl                    as V
import           Data.Vinyl.Functor            as V
import           Data.Vinyl.TypeLevel          as V
import           Frames                        as F
import           Frames                         ( (:->)
                                                , (&:)
                                                )
import           Frames.CSV                    as F
import           Frames.InCore                 as FI
import qualified Pipes                         as P

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

getACSData :: CF.Year -> ACS_Span -> CF.GeoCode a -> [Text] -> ClientM A.Value
getACSData year span geoCode codes =
  let (forM, inM) = CF.geoCodeToQuery geoCode
      getQ        = Just $ intercalate "," codes
  in  (_ACS censusClients) year
                           (acsSpanToText span)
                           getQ
                           forM
                           inM
                           (Just censusApiKey)


type QueryFieldsC d gs fs = ( CF.QueryFields d fs
                            , ColumnHeaders (CF.QueryCodes d fs)
                            , gs F.⊆ ((CF.QueryCodes d fs) V.++ gs)
                            , (CF.QueryCodes d fs) F.⊆ ((CF.QueryCodes d fs) V.++ gs)
                            , V.RecordToList ((CF.QueryCodes d fs) V.++ gs)
                            , V.ReifyConstraint Show V.ElField ((CF.QueryCodes d fs) V.++ gs)
                            , RMap ((CF.QueryCodes d fs) V.++ gs)
                            , FI.RecVec ((CF.QueryCodes d fs) V.++ gs)
                            , F.ReadRec ((CF.QueryCodes d fs) V.++ gs))

getACSDataFrame
  :: forall fs gs
   . QueryFieldsC CF.ACS gs fs
  => CF.Year
  -> ACS_Span
  -> CF.GeoCode gs
  -> ClientM (F.FrameRec (gs V.++ fs))
getACSDataFrame year span geoCode = do
  asAeson <- getACSData year span geoCode (CF.queryCodes @CF.ACS @fs)
  rawFrame :: F.FrameRec ((CF.QueryCodes CF.ACS fs) V.++ gs) <-
    liftIO
    $     FI.inCoreAoS
    $     jsonArraysToRecordPipe (return asAeson)
    P.>-> mapEither
  let f r = (F.rcast @gs r) F.<+> (CF.makeQRec @CF.ACS @fs $ F.rcast r)
  return $ fmap f rawFrame


jsonTextArrayToList :: A.Value -> [Text]
jsonTextArrayToList x = x ^.. L.values . L._String

jsonArraysOfTextToPipe
  :: (MonadIO m, Monad m) => m A.Value -> P.Producer Text m ()
jsonArraysOfTextToPipe mv = do
  v <- P.lift mv
--  liftIO $ putStrLn $ show v
  let vs = v ^.. L.values -- get rid of header row
  forM_ vs $ \v -> do
    let csv = intercalate "," $ jsonTextArrayToList v
--    liftIO $ putStr $ show csv ++ ": "
    P.yield csv
  return ()

jsonArraysToRecordPipe
  :: (F.ReadRec rs, Monad m, MonadIO m)
  => m A.Value
  -> P.Producer (F.Rec (Either Text :. F.ElField) rs) m ()
jsonArraysToRecordPipe mv = jsonArraysOfTextToPipe mv P.>-> F.pipeTableEither

mapEither
  :: ( Monad m
     , MonadIO m
     , RMap rs
     , V.ReifyConstraint Show V.ElField rs
     , V.RecordToList rs
     )
  => P.Pipe (F.Rec (Either Text :. F.ElField) rs) (F.Record rs) m ()
mapEither = do
  eRec <- P.await
--  liftIO $ eitherRecordPrint eRec
  case (F.rtraverse V.getCompose eRec) of
    Left  _ -> mapEither -- skip it
    Right r -> P.yield r >> mapEither

-- for debugging
eitherRecordPrint
  :: (V.RMap rs, V.ReifyConstraint Show V.ElField rs, V.RecordToList rs)
  => F.Rec (Either Text :. F.ElField) rs
  -> IO ()
eitherRecordPrint eRec = case (F.rtraverse V.getCompose eRec) of
  Left  err -> liftIO $ putStrLn $ "Error: " ++ (unpack err)
  Right r   -> liftIO $ print r


getSAIPEData :: CF.Year -> CF.GeoCode a -> [Text] -> ClientM A.Value
getSAIPEData year geo codes =
  let (forM, inM) = CF.geoCodeToQuery geo
  in  (_SAIPE censusClients) (Just $ intercalate "," codes)
                             forM
                             inM
                             (Just year)
                             (Just censusApiKey)


getSAIPEDataFrame
  :: forall fs gs
   . QueryFieldsC CF.SAIPE gs fs
  => CF.Year
  -> CF.GeoCode gs
  -> ClientM (F.FrameRec (gs V.++ fs))
getSAIPEDataFrame year geoCode = do
  asAeson <- getSAIPEData year geoCode (CF.queryCodes @CF.SAIPE @fs)
  rawFrame :: F.FrameRec ((CF.QueryCodes CF.SAIPE fs) V.++ gs) <-
    liftIO
    $     FI.inCoreAoS
    $     jsonArraysToRecordPipe (return asAeson)
    P.>-> mapEither
  let f r = (F.rcast @gs r) F.<+> (CF.makeQRec @CF.SAIPE @fs $ F.rcast r)
  return $ fmap f rawFrame


