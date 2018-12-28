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

import           Census.API              as Census

import           Control.Exception.Safe  (throw)
import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant
import           Servant.Client          (ClientM, ServantError, mkClientEnv,
                                          runClientM)

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Aeson              as A
import           Data.ByteString.Lazy    (fromStrict)
--import           Data.List               (catMaybe)
import           Control.Lens            ((^.))
import qualified Control.Lens            as L
import           Data.Maybe              (catMaybes, isJust)
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
  let managerSettings = tlsManagerSettings -- { managerModifyRequest  =  (\req -> putStrLn (show req) >> return req) }
  stateKeysFrame <- getStateFIPSFrame
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager Census.baseUrl
      runServant x = runClientM x clientEnv
      allStatesAndCounties = AllStatesAndCounties
      allStates = GeoCodeRawForIn "county:*" "state:01"
      query = Census.getSAIPEData 2015 allStatesAndCounties [MedianHouseholdIncome, MedianHouseholdIncomeMOE,PovertyRate]
      parsePipe :: IO A.Value -> P.Producer (F.Rec (Either F.Text F.:. F.ElField) SAIPE) IO () = jsonArraysToRecordPipe
  --_ <- flip runClientM clientEnv (P.runEffect $ P.for parsePipe (liftIO . eitherRecordPrint))
  result <- runClientM query clientEnv
  case result of
    Left err -> putStrLn $ "Query produced an error: " ++ show err
    Right x  -> do
      censusFrame <- FI.inCoreAoS $ parsePipe (return x) P.>-> mapEither
      let withStateAbbrevsF = F.toFrame $ catMaybes $ fmap F.recMaybe $ F.leftJoin @'[StateFIPS] censusFrame stateKeysFrame
          addComboFIPS r = (r^.Census.stateFIPS * 1000 + r^.Census.countyFIPS) F.&: r
          simplifiedF :: F.FrameRec '[FIPS, StateFIPS, CountyFIPS, Abbreviation, MedianHI, MedianHI_MOE, PovertyR] = addComboFIPS . F.rcast <$> withStateAbbrevsF
      F.writeCSV "data/medianHIByCounty.csv" simplifiedF
  return ()


