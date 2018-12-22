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
    _ACS :: route :- Capture "Year" Year :> "acs" :> Capture "span" ACS_Span :> QueryParams "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "key" Text :> Get '[JSON] A.Value
  }
  deriving (Generic)

censusClients :: Census_Routes (AsClientT ClientM)
censusClients = genericClient

-- OpenFEC specific constants

type ApiKey = Text
censusApiKey :: ApiKey
censusApiKey = "2a9aeb4cae45db2d5e7ce1c9d0622caf68e8de15"

type Year = Int
type ACS_Span = Text
type ACS_DataCode = Text

-- Want constructors that take smarter types, like a state/county or state/congressional district or whatever
data ACS_GeoCode where
  ACS_GeoCodeRawFor :: Text -> ACS_GeoCode
  ACS_GeoCodeRawForIn :: Text -> Text -> ACS_GeoCode

geoCodeToQuery :: ACS_GeoCode -> (Maybe Text, Maybe Text)
geoCodeToQuery (ACS_GeoCodeRawFor forText) = (Just forText, Nothing)
geoCodeToQuery (ACS_GeoCodeRawForIn forText inText) = (Just forText, Just inText)

getTestCensusData :: Year -> ACS_Span -> [ACS_DataCode] -> ACS_GeoCode -> ClientM A.Value
getTestCensusData year span codes geoCode =
  let (forM, inM) = geoCodeToQuery geoCode
  in (_ACS censusClients) year span codes forM inM (Just censusApiKey)




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
