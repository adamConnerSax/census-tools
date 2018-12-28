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
import           Data.Text.Encoding      (encodeUtf8)
import qualified Frames                  as F
import qualified Frames.CSV              as F
import qualified Frames.InCore           as FI
import qualified Pipes                   as P
import qualified Pipes.Prelude           as P
import qualified Pipes.Safe              as P

--F.tableTypes "StateFIPS" ".conversion-data/states.csv"

main :: IO ()
main = do
  let managerSettings = tlsManagerSettings { managerModifyRequest  =  (\req -> putStrLn (show req) >> return req) }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager Census.baseUrl
      runServant x = runClientM x clientEnv
      allStatesAndCounties = AllStatesAndCounties
      allStates = GeoCodeRawForIn "county:*" "state:01"
      query = Census.getSAIPEData 2015 allStates [MedianHouseholdIncome, MedianHouseholdIncomeMOE]
      parsePipe :: IO A.Value -> P.Producer (F.Rec (Either F.Text F.:. F.ElField) SAIPE) IO () = jsonArraysToRecordPipe
  --_ <- flip runClientM clientEnv (P.runEffect $ P.for parsePipe (liftIO . eitherRecordPrint))
  result <- runClientM query clientEnv
  case result of
    Left err -> putStrLn $ "Query produced an error: " ++ show err
    Right x  -> do
      frame <- FI.inCoreAoS $ parsePipe (return x) P.>-> mapEither
      let csvProducer :: P.Producer String IO () = F.produceCSV frame
      P.runEffect $ P.for csvProducer (P.lift . print)
  return ()
  {-
  result <- runClientM query clientEnv -- Either ServantError
  case result of
    Left err -> putStrLn $ "Query produced an error: " ++ show err
    Right x -> do
      putStrLn $ show x
--      putStrLn $ show
      case censusJSONToFrame x  of
        Left err -> throw $ err417 { errBody = "Decoding error parsing census API result at Frame: " <> fromStrict (encodeUtf8 err)  }
        Right f -> F.runSafeEffect $ F.produceCSV f P.>-> P.print
-}
