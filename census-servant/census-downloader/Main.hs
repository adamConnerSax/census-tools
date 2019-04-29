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
import           Servant.Client          (ClientM, ClientError, mkClientEnv,
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

--type ACSFields = [Census.Population, Census.WMY, Census.WMO, Census.WFY, Census.WFO, Census.NWMY, Census.NWMO, Census.NWFY, Census.NWFO]

--                  Census.WFO,Census.BMY,Census.BMO,Census.BFY,Census.BFO]

type ACSFields = '[Census.Population
                  ,Census.YoungWhiteMalePct
                  ,Census.OldWhiteMalePct
                  ,Census.YoungWhiteFemalePct
                  ,Census.OldWhiteFemalePct
                  ,Census.YoungNonWhiteMalePct
                  ,Census.OldNonWhiteMalePct
                  ,Census.YoungNonWhiteFemalePct
                  ,Census.OldNonWhiteFemalePct
                  ]
main :: IO ()
main = do
  let managerSettings = tlsManagerSettings { managerModifyRequest  =  (\req -> putStrLn (show req) >> return req) }
  stateKeysFrame <- Census.getStateFIPSFrame
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager Census.baseUrl
      runServant x = Census.runX clientEnv x
  resFEs <- sequence $ fmap (\x -> putStr (show x ++ "...") >> getOneYear @ACSFields runServant stateKeysFrame Census.AllStatesAndDistricts x) ([2017])
  let (errors,resFs) = partitionEithers resFEs
  case (List.null errors) of
    True  -> F.writeCSV "data/test.csv" $ mconcat resFs
    False -> putStrLn $ "Some queries returned errors: " ++ show errors
  return ()

type ACSRes gs fs = ('[Census.StateAbbreviation] V.++ gs V.++ fs)

getOneYear :: forall fs gs. (F.ColumnHeaders fs
                            , F.ColumnHeaders gs
                            , F.ReadRec (gs V.++ fs)                            
                            , V.RMap (gs V.++ fs)
                            , V.RecordToList (gs V.++ fs)
                            , V.ReifyConstraint Show F.ElField (gs V.++ fs)                            
                            , FI.RecVec (gs V.++ fs)
                            , FI.RecVec ((gs V.++ fs) V.++ '[Census.StateName, Census.StateAbbreviation])
                            , V.RMap ((gs V.++ fs) V.++ '[Census.StateName, Census.StateAbbreviation])
                            , (gs V.++ fs) F.⊆ ((gs V.++ fs) V.++ '[Census.StateName, Census.StateAbbreviation])
                            , F.ElemOf (gs V.++ fs) Census.StateFIPS
                            , F.ElemOf  ((gs V.++ fs) V.++ '[Census.StateName, Census.StateAbbreviation]) Census.StateAbbreviation
                            {-, F.Elem (fs V.++ gs) Census.StateFIPS-})
           => (Census.MyServantClientM (F.FrameRec (gs V.++ fs)) -> IO (Either Text (F.FrameRec (gs V.++ fs))))
           -> F.Frame Census.StateFIPSAndNames
           -> Census.GeoCode gs
           -> Census.Year
           -> IO (Either Text (F.FrameRec (ACSRes gs fs)))
getOneYear runServant stateKeysFrame geoCode year = do
  let acsQuery = Census.getACSDataFrame' @fs Census.acsRequestDictionary year Census.ACS1 geoCode
  result <- runServant acsQuery
  case result of
    Left err -> return $ Left err
    Right censusFrame  -> Right <$> do      
      let withStateAbbrevsF = F.toFrame $ catMaybes $ fmap F.recMaybe $ F.leftJoin @'[Census.StateFIPS] censusFrame stateKeysFrame
          simplifiedF :: F.FrameRec (ACSRes gs fs) = F.rcast <$> withStateAbbrevsF
      return simplifiedF




