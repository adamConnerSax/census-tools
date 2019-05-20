{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE Rank2Types                #-}
module Census.Fields where

import qualified Census.ComputedFields         as CF

import qualified Data.Aeson                    as A
import qualified Data.Foldable                 as Fold
import           Data.List                      ( nub
                                                , concat
                                                ) -- I know this is inefficient.  Short lists.
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Map                      as M
import qualified Data.HashMap.Lazy             as HML
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Text.Read                     as TR

import qualified Data.Set                      as S
import qualified Data.Vinyl                    as V
import qualified Data.Vinyl.Functor            as V
import qualified Data.Vinyl.TypeLevel          as V
import qualified Data.Vector                   as Vec
import qualified Frames                        as F
import qualified Frames                         ( (:->)
                                                , (&:)
                                                )

import qualified Control.Monad.Except          as X

import           Data.Kind                      ( Type )
import           GHC.TypeLits                   ( Symbol
                                                , KnownSymbol(..)
                                                )
-- state FIPS datatypes
F.tableTypes "StateFIPSAndNames" "../conversion-data/states.csv"  -- declares StateName, StateFIPS, StateAbbreviation

F.declareColumn "CountyFIPS" ''Int -- = "countyFIPS" :-> Int
F.declareColumn "CongressionalDistrict" ''Int -- = "countyFIPS" :-> Int

type YearT = Int
F.declareColumn "Year" ''YearT

data GeoCode a where
  AllStatesAndCounties :: GeoCode '[StateFIPS, CountyFIPS]
  AllStatesAndDistricts :: GeoCode '[StateFIPS, CongressionalDistrict]

resultToX :: Monad m => A.Result a -> X.ExceptT Text m a
resultToX (A.Success a) = return a
resultToX (A.Error msg) =
  X.throwError $ "Aeson decoding failure: " <> (T.pack msg)

m2X :: Monad m => Text -> Maybe a -> X.ExceptT Text m a
m2X msg = maybe (X.throwError msg) return

lookupValueX :: Monad m => Text -> A.Object -> X.ExceptT Text m A.Value
lookupValueX k = m2X ("Lookup failed for key=\"" <> k <> "\"") . HML.lookup k

lookupAndParseX :: (Monad m, Read a) => Text -> A.Object -> X.ExceptT Text m a
lookupAndParseX key m = do
  asVal    <- lookupValueX key m
  asString <- resultToX $ A.fromJSON asVal
  m2X "Error converting to Int" $ TR.readMaybe asString

-- parse the geoCode fields into a frame to act as a key and remove
-- NB: This happens before the processing wso we need to lookup
-- the names the census returns here
geoDecode
  :: Monad m => GeoCode a -> A.Object -> X.ExceptT Text m (F.Record a, A.Object)

geoDecode AllStatesAndCounties m = do
  stateFIPS  <- lookupAndParseX "state" m
  countyFIPS <- lookupAndParseX "county" m
  let gRecord   = stateFIPS F.&: countyFIPS F.&: V.RNil
      mapOfData = HML.delete "StateFIPS" $ HML.delete "CountyFIPS" $ m
  return $ (gRecord, mapOfData)

geoDecode AllStatesAndDistricts m = do
  stateFIPS <- lookupAndParseX "state" m
  district  <- lookupAndParseX "congressional district" m
  let gRecord = stateFIPS F.&: district F.&: V.RNil
      mapOfData =
        HML.delete "StateFIPS" $ HML.delete "CongressionalDistrict" $ m
  return $ (gRecord, mapOfData)


geoCodeToQuery :: GeoCode a -> (Maybe Text, Maybe Text)
--geoCodeToQuery (GeoCodeRawFor forText) = (Just forText, Nothing)
--geoCodeToQuery (GeoCodeRawForIn forText inText) = (Just forText, Just inText)
geoCodeToQuery AllStatesAndCounties = (Just "county:*", Just "state:*")
geoCodeToQuery AllStatesAndDistricts =
  (Just "congressional district:*", Just "state:*")

addGeoCodeRequests :: GeoCode a -> CF.RequestDictionary -> CF.RequestDictionary
addGeoCodeRequests x (CF.RequestDictionary dict) =
  CF.RequestDictionary $ M.union dict $ case x of
    AllStatesAndCounties -> M.fromList
      [CF.inQuery "county" "CountyFIPS", CF.inQuery "state" "StateFIPS"]
    AllStatesAndDistricts -> M.fromList
      [ CF.inQuery "congressional district" "CongressionalDistrict"
      , CF.inQuery "state" "StateFIPS"
      ]

{-
geoCodedQueryResultToValueMap
  :: (Foldable f, F.ColumnHeaders a)
  => GeoCode a -- ^ will contain 
  -> Vec.Vector Text -- ^ headers 
  -> f (Vec.Vector.Value) -- ^ data rows
  -> Vector M.Map (F.FrameRec a) (M.Map ResultKey A.Value)
geoCodedQueryResultToValueMap gc headers rows =
  let gcCols = fmap pack $ F.columnHeaders (Proxy :: Proxy (F.Record a))
      toMap h = M.fromList . Vec.toList . Vec.zip h
-}

acsGRARequestDictionary =
  CF.addRequests (allGARequests ++ allWNHRequests) CF.emptyRequestDictionary

acsGRAKeys = allWNHKeys ++ allGAKeys

data GenderT = Female | Male deriving (Enum,Bounded,Eq,Ord,Show)
data RaceT = Black | Hispanic | Asian | Native | Pacific | WhiteAlone | WhiteNonHispanic  deriving (Enum,Bounded,Eq,Ord,Show)
data AgeT = A18To24 | A25To44 | A45To64 | A65To74 | A75AndOver deriving (Enum,Bounded,Eq,Ord,Show)

ageKey :: AgeT -> CF.ResultKey
ageKey A18To24    = "18To24"
ageKey A25To44    = "25To44"
ageKey A45To64    = "45To64"
ageKey A65To74    = "65To74"
ageKey A75AndOver = "75AndOver"

graKeyText :: GenderT -> RaceT -> AgeT -> CF.ResultKey
graKeyText g r a = T.pack (show g) <> T.pack (show r) <> ageKey a

gaKeyText :: GenderT -> AgeT -> CF.ResultKey
gaKeyText g a = T.pack (show g) <> ageKey a

raceCode :: RaceT -> Text
raceCode Black            = "B"
raceCode Hispanic         = "I"
raceCode Asian            = "D"
raceCode WhiteNonHispanic = "H"
raceCode WhiteAlone       = "A"
raceCode Native           = "C"
raceCode Pacific          = "E"

zeroPaddedText :: Int -> Int -> Text
zeroPaddedText numChars n =
  let s = show n
      m = max 0 $ numChars - length s
  in  T.pack $ replicate m '0' ++ s

gaRequests :: GenderT -> AgeT -> CF.ResultKey -> [CF.Request]
gaRequests g a key =
  let code_numbers = case (g, a) of
        (Female, A18To24   ) -> [31 .. 34]
        (Female, A25To44   ) -> [35 .. 38]
        (Female, A45To64   ) -> [39 .. 43]
        (Female, A65To74   ) -> [44 .. 46]
        (Female, A75AndOver) -> [47 .. 49]
        (Male  , A18To24   ) -> [7 .. 10]
        (Male  , A25To44   ) -> [11 .. 14]
        (Male  , A45To64   ) -> [15 .. 19]
        (Male  , A65To74   ) -> [20 .. 22]
        (Male  , A75AndOver) -> [23 .. 25]
      queryCodes = fmap (\cn -> "B01001_" <> cn <> "E")
                        (fmap (zeroPaddedText 3) code_numbers)
      queryRequests   = concat $ fmap (\x -> CF.query x x) queryCodes
      computedRequest = CF.addAll key (S.fromList queryCodes)
  in  computedRequest : queryRequests

graRequests :: GenderT -> RaceT -> AgeT -> CF.ResultKey -> [CF.Request]
graRequests g r a key =
  let rc           = raceCode r
      code_numbers = case (g, a) of
        (Female, A18To24   ) -> [22 .. 23]
        (Female, A25To44   ) -> [24 .. 26]
        (Female, A45To64   ) -> [27 .. 28]
        (Female, A65To74   ) -> [29]
        (Female, A75AndOver) -> [30, 31]
        (Male  , A18To24   ) -> [7, 8]
        (Male  , A25To44   ) -> [9 .. 11]
        (Male  , A45To64   ) -> [12, 13]
        (Male  , A65To74   ) -> [14]
        (Male  , A75AndOver) -> [15, 16]
      queryCodes = fmap (\cn -> "B01001" <> rc <> "_" <> cn <> "E")
                        (fmap (zeroPaddedText 3) code_numbers)
      queryRequests   = concat $ fmap (\x -> CF.query x x) queryCodes
      computedRequest = CF.addAll key (S.fromList queryCodes)
  in  computedRequest : queryRequests

allGRARequests :: [CF.Request]
allGRARequests = concat $ do
  g :: GenderT <- [minBound ..]
  r :: RaceT   <- [minBound ..]
  a :: AgeT    <- [minBound ..]
  return $ graRequests g r a (graKeyText g r a)

allGRAKeys :: [CF.ResultKey]
allGRAKeys = do
  g :: GenderT <- [minBound ..]
  r :: RaceT   <- [minBound ..]
  a :: AgeT    <- [minBound ..]
  return $ graKeyText g r a

allWNHRequests :: [CF.Request]
allWNHRequests = concat $ do
  let r = WhiteNonHispanic
  g :: GenderT <- [minBound ..]
  a :: AgeT    <- [minBound ..]
  return $ graRequests g r a (graKeyText g r a)

allWNHKeys :: [CF.ResultKey]
allWNHKeys = do
  let r = WhiteNonHispanic
  g :: GenderT <- [minBound ..]
  a :: AgeT    <- [minBound ..]
  return $ graKeyText g r a


allGARequests :: [CF.Request]
allGARequests = concat $ do
  g :: GenderT <- [minBound ..]
  a :: AgeT    <- [minBound ..]
  return $ gaRequests g a (gaKeyText g a)

allGAKeys :: [CF.ResultKey]
allGAKeys = do
  g :: GenderT <- [minBound ..]
  a :: AgeT    <- [minBound ..]
  return $ gaKeyText g a
