{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Main where

import qualified Census.API              as Census

import           Control.Exception.Safe  (throw)
import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant
import           Servant.Client          (ClientM, ServantError, mkClientEnv,
                                          runClientM)

import           Control.Lens            ((^.))
import qualified Control.Lens            as L
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Aeson              as A
import           Data.ByteString.Lazy    (fromStrict)
import           Data.Either             (partitionEithers)
import qualified Data.List               as List
import           Data.Maybe              (catMaybes, isJust)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Frames                  as F
import qualified Frames.CSV              as F
import qualified Frames.InCore           as FI
import qualified Pipes                   as P
import qualified Pipes.Prelude           as P
import qualified Pipes.Safe              as P

type FIPS = "fips" F.:-> Int

main :: IO ()
main = do
  let managerSettings = tlsManagerSettings { managerModifyRequest  =  (\req -> putStrLn (show req) >> return req) }
  stateKeysFrame <- Census.getStateFIPSFrame
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager Census.baseUrl
      runServant x = runClientM x clientEnv
  resFEs <- sequence $ fmap (\x -> putStr (show x ++ "...") >> getOneYear runServant stateKeysFrame x) ([2016])
  let (errors,resFs) = partitionEithers resFEs
  case (List.null errors) of
    True  -> F.writeCSV "data/medianHIByDistrict.csv" $ mconcat resFs
    False -> putStrLn $ "Some queries returned errors: " ++ show errors
  return ()


getOneYear :: (ClientM A.Value -> IO (Either ServantError A.Value))
           -> F.Frame Census.StateFIPSAndNames
           -> Census.Year
           -> IO (Either ServantError (F.FrameRec '[Census.StateFIPS
                                                   , Census.CongressionalDistrict
                                                   , Census.Abbreviation
                                                   , Census.YearF
                                                   , Census.MedianHI
                                                   --                                                   , Census.MedianHI_MOE
                                                   --                                          , Census.PovertyR
                                                   ]))
getOneYear runServant stateKeysFrame year = do
  let allStatesAndDistricts = Census.AllStatesAndDistricts
      allInAlabama = Census.GeoCodeRawForIn "county:*" "state:01"
      acsQuery
        = Census.getACSData year Census.ACS1 Census.AllStatesAndDistricts
          [
            "B19013" -- Census.ACS_MedianHouseholdIncome
          ]
      saipeQuery = Census.getSAIPEData year Census.AllStatesAndDistricts [ Census.MedianHouseholdIncome
                                                                         , Census.MedianHouseholdIncomeMOE
                                                                         , Census.PovertyRate]
      parsePipe :: IO A.Value -> P.Producer (F.Rec (Either F.Text F.:. F.ElField) Census.ACS) IO () = Census.jsonArraysToRecordPipe
  result <- runServant acsQuery
  case result of
    Left err -> return $ Left err
    Right x  -> Right <$> do
      censusFrame <- FI.inCoreAoS $ parsePipe (return x) P.>-> Census.mapEither
      let withStateAbbrevsF = F.toFrame $ catMaybes $ fmap F.recMaybe $ F.leftJoin @'[Census.StateFIPS] censusFrame stateKeysFrame
--          addComboFIPS r = (r^.Census.stateFIPS * 1000 + r^.Census.countyFIPS) F.&: r
          simplifiedF :: F.FrameRec '[Census.StateFIPS
                                     , Census.CongressionalDistrict
                                     , Census.Abbreviation
                                     , Census.YearF
                                     , Census.MedianHI
                                     --                                     , Census.MedianHI_MOE
                                     --                                     , Census.PovertyR
                                     ] = F.rcast <$> withStateAbbrevsF
      return simplifiedF




