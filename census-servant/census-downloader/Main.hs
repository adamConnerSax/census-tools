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
{-# LANGUAGE AllowAmbiguousTypes       #-}
module Main where

import qualified Census.API              as Census
import qualified Census.Fields           as Census

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
import qualified Frames.Melt             as F -- for Elem
import qualified Frames.InCore           as FI
import qualified Data.Vinyl.TypeLevel    as V
import qualified Data.Vinyl.Core         as V
import qualified Pipes                   as P
import qualified Pipes.Prelude           as P
import qualified Pipes.Safe              as P

type ACSFields = [Census.Population, Census.MedianHouseholdIncome, Census.MedianAge, Census.CollegeGrads, Census.AverageHouseholdSize, Census.PovertyCount]

main :: IO ()
main = do
  let managerSettings = tlsManagerSettings { managerModifyRequest  =  (\req -> putStrLn (show req) >> return req) }
  stateKeysFrame <- Census.getStateFIPSFrame
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager Census.baseUrl
      runServant x = runClientM x clientEnv
  resFEs <- sequence $ fmap (\x -> putStr (show x ++ "...") >> getOneYear @ACSFields runServant stateKeysFrame Census.AllStatesAndDistricts x) ([2017])
  let (errors,resFs) = partitionEithers resFEs
  case (List.null errors) of
    True  -> F.writeCSV "data/popByDistrict.csv" $ mconcat resFs
    False -> putStrLn $ "Some queries returned errors: " ++ show errors
  return ()

type ACSRes gs fs = ('[Census.StateAbbreviation] V.++ gs V.++ fs)

getOneYear :: forall fs gs. (Census.ACSQueryFields fs gs
                            , FI.RecVec ((fs V.++ gs) V.++ '[Census.StateName, Census.StateAbbreviation])
                            , V.RMap ((fs V.++ gs) V.++ '[Census.StateName, Census.StateAbbreviation])
                            , (fs V.++ gs) F.⊆ ((fs V.++ gs) V.++ '[Census.StateName, Census.StateAbbreviation])
                            , (gs V.++ fs) F.⊆ ((fs V.++ gs) V.++ '[Census.StateName, Census.StateAbbreviation])
                            , F.ElemOf (fs V.++ gs) Census.StateFIPS
                            , F.ElemOf  ((fs V.++ gs) V.++ '[Census.StateName, Census.StateAbbreviation]) Census.StateAbbreviation
                            {-, F.Elem (fs V.++ gs) Census.StateFIPS-})
           => (ClientM (F.FrameRec (fs V.++ gs)) -> IO (Either ServantError (F.FrameRec (fs V.++ gs))))
           -> F.Frame Census.StateFIPSAndNames
           -> Census.GeoCode gs
           -> Census.Year
           -> IO (Either ServantError (F.FrameRec (ACSRes gs fs)))
getOneYear runServant stateKeysFrame geoCode year = do
  let acsQuery = Census.getACSDataFrame @fs year Census.ACS1 geoCode
  result <- runServant acsQuery
  case result of
    Left err -> return $ Left err
    Right censusFrame  -> Right <$> do      
      let withStateAbbrevsF = F.toFrame $ catMaybes $ fmap F.recMaybe $ F.leftJoin @'[Census.StateFIPS] censusFrame stateKeysFrame
          simplifiedF :: F.FrameRec (ACSRes gs fs) = F.rcast <$> withStateAbbrevsF
      return simplifiedF




