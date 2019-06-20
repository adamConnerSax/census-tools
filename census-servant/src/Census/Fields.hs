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
data AgeRT = A18To24 | A25To44 | A45To64 | A65To74 | A75AndOver deriving (Enum,Bounded,Eq,Ord,Show)

data EducationT = LessThan9th | LessThan12th | HighSchool | SomeCollege | Associates | Bachelors | AdvancedDegree deriving (Enum, Bounded, Eq, Ord, Show)
data AgeET = AE18To24 | AE25To44 | AE45To64 | AE65AndOver deriving (Enum, Bounded, Eq, Ord, Show)

ageRKey :: AgeRT -> CF.ResultKey
ageRKey A18To24    = "18To24"
ageRKey A25To44    = "25To44"
ageRKey A45To64    = "45To64"
ageRKey A65To74    = "65To74"
ageRKey A75AndOver = "75AndOver"

ageEKey :: AgeET -> CF.ResultKey
ageEKey AE18To24    = "18To24"
ageEKey AE25To44    = "25To44"
ageEKey AE45To64    = "45To64"
ageEKey AE65AndOver    = "65AndOver"

graKeyText :: GenderT -> RaceT -> AgeRT -> CF.ResultKey
graKeyText g r a = T.pack (show g) <> T.pack (show r) <> ageRKey a

gaKeyText :: GenderT -> AgeRT -> CF.ResultKey
gaKeyText g a = T.pack (show g) <> ageRKey a

gaeKeyText :: GenderT -> AgeET -> EducationT -> CF.ResultKey
gaeKeyText g a e = T.pack (show g) <> ageEKey a <> T.pack (show e)  

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

-- NB: These are each one request but lets keep the signatures similar 
gaeRequests :: GenderT -> AgeET -> EducationT -> CF.ResultKey -> [CF.Request]
gaeRequests g a e key =
  let code_numbers = case (g, a, e) of
        (Male, AE18To24, LessThan9th) -> [4]
        (Male, AE18To24, LessThan12th) -> [5]
        (Male, AE18To24, HighSchool) -> [6]
        (Male, AE18To24, SomeCollege) -> [7]
        (Male, AE18To24, Associates) -> [8]
        (Male, AE18To24, Bachelors) -> [9]
        (Male, AE18To24, AdvancedDegree) -> [10]
        (Male, AE25To44, LessThan9th) -> [12,20]
        (Male, AE25To44, LessThan12th) -> [13,21]
        (Male, AE25To44, HighSchool) -> [14,22]
        (Male, AE25To44, SomeCollege) -> [15,23]
        (Male, AE25To44, Associates) -> [16,24]
        (Male, AE25To44, Bachelors) -> [17,25]
        (Male, AE25To44, AdvancedDegree) -> [18,26]
        (Male, AE45To64, LessThan9th) -> [28]
        (Male, AE45To64, LessThan12th) -> [29]
        (Male, AE45To64, HighSchool) -> [30]
        (Male, AE45To64, SomeCollege) -> [31]
        (Male, AE45To64, Associates) -> [32]
        (Male, AE45To64, Bachelors) -> [33]
        (Male, AE45To64, AdvancedDegree) -> [34]
        (Male, AE65AndOver, LessThan9th) -> [36]
        (Male, AE65AndOver, LessThan12th) -> [37]
        (Male, AE65AndOver, HighSchool) -> [38]
        (Male, AE65AndOver, SomeCollege) -> [39]
        (Male, AE65AndOver, Associates) -> [40]
        (Male, AE65AndOver, Bachelors) -> [41]
        (Male, AE65AndOver, AdvancedDegree) -> [42]
        (Female, AE18To24, LessThan9th) -> [45]
        (Female, AE18To24, LessThan12th) -> [46]
        (Female, AE18To24, HighSchool) -> [47]
        (Female, AE18To24, SomeCollege) -> [48]
        (Female, AE18To24, Associates) -> [49]
        (Female, AE18To24, Bachelors) -> [50]
        (Female, AE18To24, AdvancedDegree) -> [51]
        (Female, AE25To44, LessThan9th) -> [53,61]
        (Female, AE25To44, LessThan12th) -> [54,62]
        (Female, AE25To44, HighSchool) -> [55,63]
        (Female, AE25To44, SomeCollege) -> [56,64]
        (Female, AE25To44, Associates) -> [57,65]
        (Female, AE25To44, Bachelors) -> [58,66]
        (Female, AE25To44, AdvancedDegree) -> [59,67]
        (Female, AE45To64, LessThan9th) -> [69]
        (Female, AE45To64, LessThan12th) -> [70]
        (Female, AE45To64, HighSchool) -> [71]
        (Female, AE45To64, SomeCollege) -> [72]
        (Female, AE45To64, Associates) -> [73]
        (Female, AE45To64, Bachelors) -> [74]
        (Female, AE45To64, AdvancedDegree) -> [75]
        (Female, AE65AndOver, LessThan9th) -> [77]
        (Female, AE65AndOver, LessThan12th) -> [78]
        (Female, AE65AndOver, HighSchool) -> [79]
        (Female, AE65AndOver, SomeCollege) -> [80]
        (Female, AE65AndOver, Associates) -> [81]
        (Female, AE65AndOver, Bachelors) -> [82]
        (Female, AE65AndOver, AdvancedDegree) -> [83]
      queryCodes = fmap (\cn -> "B15001_" <> cn <> "E")
                        (fmap (zeroPaddedText 3) code_numbers)
      queryRequests   = concat $ fmap (\x -> CF.query x x) queryCodes
      computedRequest = CF.addAll key (S.fromList queryCodes)
  in  computedRequest : queryRequests
          
gaRequests :: GenderT -> AgeRT -> CF.ResultKey -> [CF.Request]
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

graRequests :: GenderT -> RaceT -> AgeRT -> CF.ResultKey -> [CF.Request]
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
  a :: AgeRT    <- [minBound ..]
  return $ graRequests g r a (graKeyText g r a)

allGRAKeys :: [CF.ResultKey]
allGRAKeys = do
  g :: GenderT <- [minBound ..]
  r :: RaceT   <- [minBound ..]
  a :: AgeRT    <- [minBound ..]
  return $ graKeyText g r a

allWNHRequests :: [CF.Request]
allWNHRequests = concat $ do
  let r = WhiteNonHispanic
  g :: GenderT <- [minBound ..]
  a :: AgeRT    <- [minBound ..]
  return $ graRequests g r a (graKeyText g r a)

allWNHKeys :: [CF.ResultKey]
allWNHKeys = do
  let r = WhiteNonHispanic
  g :: GenderT <- [minBound ..]
  a :: AgeRT    <- [minBound ..]
  return $ graKeyText g r a

allGARequests :: [CF.Request]
allGARequests = concat $ do
  g :: GenderT <- [minBound ..]
  a :: AgeRT    <- [minBound ..]
  return $ gaRequests g a (gaKeyText g a)

allGAKeys :: [CF.ResultKey]
allGAKeys = do
  g :: GenderT <- [minBound ..]
  a :: AgeRT    <- [minBound ..]
  return $ gaKeyText g a

allGAERequests :: [CF.Request]
allGAERequests = concat $ do
  g :: GenderT <- [minBound..]
  a :: AgeET <- [minBound..]
  e :: EducationT <- [minBound..]
  return $ gaeRequests g a e (gaeKeyText g a e)

allGAEKeys :: [CF.ResultKey]
allGAEKeys = do
  g :: GenderT <- [minBound..]
  a :: AgeET <- [minBound..]
  e :: EducationT <- [minBound..]
  return $ gaeKeyText g a e
