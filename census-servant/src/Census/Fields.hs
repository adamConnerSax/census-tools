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
module Census.Fields where


import           Data.Proxy (Proxy(..))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Vinyl           as V
import qualified Data.Vinyl.Functor   as V
import qualified Data.Vinyl.TypeLevel as V
import qualified Frames               as F
import qualified Frames               ((:->), (&:))

import           Data.Kind            (Type)
import           GHC.TypeLits         (Symbol, KnownSymbol(..))

-- state FIPS datatypes
F.tableTypes "StateFIPSAndNames" "../conversion-data/states.csv"  -- declares StateName, StateFIPS, StateAbbreviation

F.declareColumn "CountyFIPS" ''Int -- = "countyFIPS" :-> Int
F.declareColumn "CongressionalDistrict" ''Int -- = "countyFIPS" :-> Int

type Year = Int
F.declareColumn "YearF" ''Year

data DataSet = ACS | SAIPE deriving (Show)

-- Want constructors that take smarter types, like a state/county or state/congressional district or whatever
data GeoCode a where
  AllStatesAndCounties :: GeoCode '[StateFIPS, CountyFIPS]
  AllStatesAndDistricts :: GeoCode '[StateFIPS, CongressionalDistrict]

geoCodeToQuery :: GeoCode a -> (Maybe Text, Maybe Text)
--geoCodeToQuery (GeoCodeRawFor forText) = (Just forText, Nothing)
--geoCodeToQuery (GeoCodeRawForIn forText inText) = (Just forText, Just inText)
geoCodeToQuery AllStatesAndCounties = (Just "county:*", Just "state:*")
geoCodeToQuery AllStatesAndDistricts = (Just "congressional district:*", Just "state:*")
{-
data ACS_DataFieldCode a where
  ACS_IntField :: T.Text -> ACS_DataFieldCode Int
  ACS_DoubleField :: T.Text -> ACS_DataFieldCode Double
  ACS_TextField :: T.Text -> ACS_DataFieldCode T.Text
-}

class QueryField (b :: DataSet) (a :: (Symbol,Type)) where
  type FieldCodes b a :: [(Symbol, Type)] -- these will be like ("B17001_001E" :: KnownSymbol,Int)
  makeField :: F.Record (FieldCodes b a) -> V.Snd a


-- TODO (maybe):  have makeRec take a set of fields to keep?
-- As in: makeRec :: forall gs rs. (gs F.⊆ rs, ACSQueryCodes F.⊆ rs) => F.Rec rs -> F.Rec (gs V.++ as) 
class QueryFields  (b :: DataSet) (as ::[(Symbol,Type)]) where
  type QueryCodes b as :: [(Symbol, Type)]
  makeRec :: F.Record (QueryCodes b as) -> F.Record as

queryCodes :: forall b rs. (QueryFields b rs, F.ColumnHeaders (QueryCodes b rs)) => [T.Text]
queryCodes = T.pack <$> F.columnHeaders (Proxy :: Proxy (F.Record (QueryCodes b rs)))

instance QueryFields b '[] where
  type QueryCodes b '[] = '[]
  makeRec _ = V.RNil  

instance (V.KnownField r, QueryFields b rs, QueryField b r, (FieldCodes b r) F.⊆ (QueryCodes b (r ': rs)), (QueryCodes b rs) F.⊆ (QueryCodes b (r ': rs))) => QueryFields b (r ': rs) where
  type QueryCodes b (r ': rs) = (FieldCodes b r) V.++ (QueryCodes b rs)
  makeRec qs =
    let newField = V.Field (makeField @b @r $ F.rcast qs)
    in newField V.:& (makeRec @b @rs $ F.rcast qs)

F.declareColumn "Population" ''Int
type B01003_001E = "B01003_001E" F.:-> Int
instance QueryField ACS Population where
  type FieldCodes ACS Population = '[B01003_001E]
  makeField = F.rgetField @B01003_001E 

F.declareColumn "MedianHouseholdIncome" ''Double
type B19013_001E = "B19013_001E" F.:-> Int
instance QueryField ACS MedianHouseholdIncome where
  type FieldCodes ACS MedianHouseholdIncome = '[B19013_001E]
  makeField = realToFrac . F.rgetField @B19013_001E

type SAEMHI_PT = "SAEMHI_PT" F.:-> Double  
instance QueryField SAIPE MedianHouseholdIncome where
  type FieldCodes SAIPE MedianHouseholdIncome = '[SAEMHI_PT]
  makeField = F.rgetField @SAEMHI_PT

F.declareColumn "MedianAge" ''Double
type B01002_001E = "B01002_001E" F.:-> Double
instance QueryField ACS MedianAge where
  type FieldCodes ACS MedianAge = '[B01002_001E]
  makeField = F.rgetField @B01002_001E

F.declareColumn "CollegeGradPct" ''Double
type B06009_001E = "B06009_001E" F.:-> Int
type B06009_005E = "B06009_005E" F.:-> Int
instance QueryField ACS CollegeGradPct where
  type FieldCodes ACS CollegeGradPct = '[B06009_001E,B06009_005E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        grads = realToFrac $ F.rgetField @B06009_005E r
    in grads/tot

F.declareColumn "MedianHouseholdIncomeMOE" ''Int
type SAEMHI_MOE = "SAEMHI_MOE" F.:-> Int
instance QueryField SAIPE MedianHouseholdIncomeMOE where
  type FieldCodes SAIPE MedianHouseholdIncomeMOE = '[SAEMHI_MOE]
  makeField = F.rgetField @SAEMHI_MOE

F.declareColumn "PovertyRate" ''Double -- PovertyR = "povertyR" :-> Double
type B17001_001E = "B17001_001E" F.:-> Int
type B17001_002E = "B17001_002E" F.:-> Int
instance QueryField ACS PovertyRate where
  type FieldCodes ACS PovertyRate = '[B17001_001E,B17001_002E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B17001_001E r
        below = realToFrac $ F.rgetField @B17001_002E r
    in below/tot

type SAEPOVRTALL_PT = "SAEPOVRTALL_PT" F.:-> Double
instance QueryField SAIPE PovertyRate where
  type FieldCodes SAIPE PovertyRate = '[SAEPOVRTALL_PT]
  makeField = F.rgetField @SAEPOVRTALL_PT

F.declareColumn "AverageHouseholdSize" ''Double
type B25010_001E = "B25010_001E" F.:-> Double
instance QueryField ACS AverageHouseholdSize where
  type FieldCodes ACS AverageHouseholdSize = '[B25010_001E] -- this is avg household size *of occuped housing units* which is not quite what we want
  makeField = F.rgetField @B25010_001E

---




data SAIPEDataCode = MedianHouseholdIncome |
                     MedianHouseholdIncomeMOE |
                     Poverty0to17Rate |
                     Poverty0to17Count |
                     Poverty0to17MOE |
                     Poverty0to4Rate |
                     Poverty0to4Count |
                     Poverty0to4MOE |
                     PovertyRate |
                     PovertyCount |
                     PovertyMOE

saipeDataCodeToText MedianHouseholdIncome    = "SAEMHI_PT"
saipeDataCodeToText MedianHouseholdIncomeMOE = "SAEMHI_MOE"
saipeDataCodeToText Poverty0to17Rate         = "SAEPOVRTO_17_PT"
saipeDataCodeToText Poverty0to17Count        = "SAEPOV0_17_PT"
saipeDataCodeToText Poverty0to17MOE          = "SAEPOV0_17_MOE"
saipeDataCodeToText Poverty0to4Rate          = "SAEPOVRT0_4_PT"
saipeDataCodeToText Poverty0to4Count         = "SAEPOVT0_4_PT"
saipeDataCodeToText Poverty0to4MOE           = "SAEPOV0_4_MOE"
saipeDataCodeToText PovertyRate              = "SAEPOVRTALL_PT"
saipeDataCodeToText PovertyCount             = "SAEPOVALL_PT"
saipeDataCodeToText PovertyMOE               = "SAEPOVTALL_MOE"



