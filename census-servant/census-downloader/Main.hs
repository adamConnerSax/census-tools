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
  let managerSettings = tlsManagerSettings -- { managerModifyRequest  =  (\req -> putStrLn (show req) >> return req) }
  stateKeysFrame <- getStateFIPSFrame
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager Census.baseUrl
      runServant x = runClientM x clientEnv
  resFEs <- sequence $ fmap (\x -> putStr (show x ++ "...") >> getOneYear runServant stateKeysFrame x) ([1989,1993] ++ [1995..2017])
  let (errors,resFs) = partitionEithers resFEs
  case (List.null errors) of
    True  -> F.writeCSV "data/medianHIByCounty.csv" $ mconcat resFs
    False -> putStrLn $ "Some queries returned errors: " ++ show errors
  return ()


getOneYear :: (ClientM A.Value -> IO (Either ServantError A.Value))
           -> F.Frame StateFIPSAndNames
           -> Year
           -> IO (Either ServantError (F.FrameRec '[FIPS, StateFIPS, CountyFIPS, Abbreviation, YearF, MedianHI, MedianHI_MOE, PovertyR]))
getOneYear runServant stateKeysFrame year = do
  let allStatesAndCounties = AllStatesAndCounties
      allInAlabama = GeoCodeRawForIn "county:*" "state:01"
      query = Census.getSAIPEData year allStatesAndCounties [MedianHouseholdIncome, MedianHouseholdIncomeMOE,PovertyRate]
      parsePipe :: IO A.Value -> P.Producer (F.Rec (Either F.Text F.:. F.ElField) SAIPE) IO () = jsonArraysToRecordPipe
  result <- runServant query
  case result of
    Left err -> return $ Left err
    Right x  -> Right <$> do
      censusFrame <- FI.inCoreAoS $ parsePipe (return x) P.>-> mapEither
      let withStateAbbrevsF = F.toFrame $ catMaybes $ fmap F.recMaybe $ F.leftJoin @'[StateFIPS] censusFrame stateKeysFrame
          addComboFIPS r = (r^.Census.stateFIPS * 1000 + r^.Census.countyFIPS) F.&: r
          simplifiedF :: F.FrameRec '[FIPS, StateFIPS, CountyFIPS, Abbreviation, YearF, MedianHI, MedianHI_MOE, PovertyR] = addComboFIPS . F.rcast <$> withStateAbbrevsF
      return simplifiedF




