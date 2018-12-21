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

import Census.API as Census

import           Network.HTTP.Client                      (Manager, defaultManagerSettings,
                                                           managerModifyRequest,
                                                           newManager)
import           Network.HTTP.Client.TLS                  (tlsManagerSettings)
import           Servant.Client                           (ClientM,
                                                           ServantError,
                                                           mkClientEnv,
                                                           runClientM)

main :: IO ()
main = do
  let managerSettings = tlsManagerSettings { managerModifyRequest  =  putStrLn req >> return req }
      clientEnv = mkClientEnv manager Censes.baseUrl
