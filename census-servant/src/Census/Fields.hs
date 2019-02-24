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
module Census.Fields where


import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Vinyl           as V
import qualified Data.Vinyl.Functor   as V
import qualified Data.Vinyl.TypeLevel as V
import qualified Frames               as F
import qualified Frames               ((:->), (&:))


-- state FIPS datatypes
F.tableTypes "StateFIPSAndNames" "../conversion-data/states.csv"  -- declares StateName, StateFIPS, StateAbbreviation

--declareColumn "StateFIPS" ''Int

F.declareColumn "CountyFIPS" ''Int -- = "countyFIPS" :-> Int
F.declareColumn "CongressionalDistrict" ''Int -- = "countyFIPS" :-> Int

type Year = Int
F.declareColumn "YearF" ''Year


type ACS_DataCode = Text

--data GeoQueryItem a = AllInCategory | Only a
--type GeoSpecifier a = Maybe Text -- phantom type to hold field

-- Want constructors that take smarter types, like a state/county or state/congressional district or whatever
data GeoCode a where
  AllStatesAndCounties :: GeoCode '[StateFIPS, CountyFIPS]
  AllStatesAndDistricts :: GeoCode '[StateFIPS, CongressionalDistrict]

geoCodeToQuery :: GeoCode a -> (Maybe Text, Maybe Text)
--geoCodeToQuery (GeoCodeRawFor forText) = (Just forText, Nothing)
--geoCodeToQuery (GeoCodeRawForIn forText inText) = (Just forText, Just inText)
geoCodeToQuery AllStatesAndCounties = (Just "county:*", Just "state:*")
geoCodeToQuery AllStatesAndDistricts = (Just "congressional district:*", Just "state:*")

class ACS_Code a where
  acsCode :: ACS_DataCode

class ACS_CodeList rs where
  acsCodeList :: [ACS_DataCode]

instance ACS_CodeList '[] where
  acsCodeList = []

instance (ACS_CodeList rs, ACS_Code r) => ACS_CodeList (r ': rs) where
  acsCodeList = acsCode @r : acsCodeList @rs

class SAIPE_Code a where
  saipeCode :: T.Text

class SAIPE_CodeList a where
  saipeCodeList :: [Text]

instance SAIPE_CodeList '[] where
  saipeCodeList = []

instance (SAIPE_CodeList rs, SAIPE_Code r) => SAIPE_CodeList (r ': rs) where
  saipeCodeList = saipeCode @r : saipeCodeList @rs

F.declareColumn "Population" ''Int
instance ACS_Code Population where
  acsCode = "B01003_001E"

F.declareColumn "MedianHouseholdIncome" ''Double
instance ACS_Code MedianHouseholdIncome where
  acsCode = "B19013_001E"
instance SAIPE_Code MedianHouseholdIncome where
  saipeCode = "SAEMHI_PT"

F.declareColumn "MedianAge" ''Double
instance ACS_Code MedianAge where
  acsCode = "B01002_001E"

F.declareColumn "CollegeGrads" ''Int
instance ACS_Code CollegeGrads where
  acsCode = "B06009_005E"

F.declareColumn "MedianHouseholdIncomeMOE" ''Int
instance SAIPE_Code MedianHouseholdIncomeMOE where
  saipeCode = "SAEMHI_MOE"

F.declareColumn "PovertyRate" ''Double -- PovertyR = "povertyR" :-> Double
instance SAIPE_Code PovertyRate where
  saipeCode = "SAEPOVRTALL_PT"


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



