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
module Main where

import           Census.API              as Census

import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientM, ServantError, mkClientEnv,
                                          runClientM)

import qualified           Frames                  as F
import qualified          Frames.CSV                  as F
import qualified           Pipes as P
import qualified          Pipes.Prelude as P

--F.tableTypes "StateFIPS" ".conversion-data/states.csv"

main :: IO ()
main = do
  let managerSettings = tlsManagerSettings { managerModifyRequest  =  (\req -> putStrLn (show req) >> return req) }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager Census.baseUrl
      runServant x = runClientM x clientEnv
      query = Census.getSAIPEData 2015 AllStatesAndCounties [MedianHouseholdIncome]
  result <- runClientM query clientEnv
  case result of
    Left err -> putStrLn $ "Query returned an error: " ++ show err
    Right x  -> F.runSafeEffect $ F.produceCSV x P.>-> P.print
