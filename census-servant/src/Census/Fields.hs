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


import qualified Data.Foldable                 as Fold
import           Data.List                      ( nub ) -- I know this is inefficient.  Short lists.
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Type.Set                 as TS
import qualified Data.Vinyl                    as V
import qualified Data.Vinyl.Functor            as V
import qualified Data.Vinyl.TypeLevel          as V
import qualified Frames                        as F
import qualified Frames                         ( (:->)
                                                , (&:)
                                                )

import           Data.Kind                      ( Type )
import           GHC.TypeLits                   ( Symbol
                                                , KnownSymbol(..)
                                                )

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
geoCodeToQuery AllStatesAndDistricts =
  (Just "congressional district:*", Just "state:*")
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

queryCodes
  :: forall b rs
   . (QueryFields b rs, F.ColumnHeaders (QueryCodes b rs))
  => [T.Text]
queryCodes =
  T.pack <$> F.columnHeaders (Proxy :: Proxy (F.Record (QueryCodes b rs)))


{-
We will repeat fields and the census endpoint will get tired of us.  So we need to query only unique fields.  Which, here, requires type-level nub.

Should be in Type-List.  Got from <https://mail.haskell.org/pipermail/haskell-cafe/2016-February/123005.html> and
more specifically, <https://gist.github.com/roelvandijk/f115c6b85a3961e1b689>
-}
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Remove x xs)

type family Remove x xs where
  Remove x '[]       = '[]
  Remove x (x ': ys) =      Remove x ys
  Remove x (y ': ys) = y ': Remove x ys

instance QueryFields b '[] where
  type QueryCodes b '[] = '[]
  makeRec _ = V.RNil

instance (V.KnownField r, QueryFields b rs, QueryField b r, (FieldCodes b r) F.⊆ (QueryCodes b (r ': rs)), (QueryCodes b rs) F.⊆ (QueryCodes b (r ': rs))) => QueryFields b (r ': rs) where
  type QueryCodes b (r ': rs) = Nub ((FieldCodes b r) V.++ (QueryCodes b rs)) -- don't duplicate fields.
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


F.declareColumn "HSGradPct" ''Double
type B06009_001E = "B06009_001E" F.:-> Int
type B06009_003E = "B06009_003E" F.:-> Int
instance QueryField ACS HSGradPct where
  type FieldCodes ACS HSGradPct = '[B06009_001E,B06009_003E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        hsGrads = realToFrac $ F.rgetField @B06009_003E r
    in hsGrads/tot

F.declareColumn "NonHSGradPct" ''Double
type B06009_002E = "B06009_002E" F.:-> Int
instance QueryField ACS NonHSGradPct where
  type FieldCodes ACS NonHSGradPct = '[B06009_001E,B06009_002E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        nonHSGrads = realToFrac $ F.rgetField @B06009_002E r
    in nonHSGrads/tot

F.declareColumn "CollegeGradPct" ''Double
type B06009_005E = "B06009_005E" F.:-> Int
instance QueryField ACS CollegeGradPct where
  type FieldCodes ACS CollegeGradPct = '[B06009_001E,B06009_005E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        collegeGrads = realToFrac $ F.rgetField @B06009_005E r
    in collegeGrads/tot

F.declareColumn "GradSchoolPct" ''Double
type B06009_006E = "B06009_006E" F.:-> Int
instance QueryField ACS GradSchoolPct where
  type FieldCodes ACS GradSchoolPct = '[B06009_001E,B06009_006E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        gradSchool = realToFrac $ F.rgetField @B06009_006E r
    in gradSchool/tot

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


--
{-
The next bunch are for a trial demographic election result model.
Race x Sex x Age
Race is one of W(hite), B(lack), L(atino), A(sian), O(ther)
Sex is one of M(ale), F(emale)
Age is one of Y(oung) (<45) or O(old) (>= 45)

I am aware that these categories are exclusive and othering.  That sucks.  I've got two problems.  I need to shrink the number of categories to
do anything meaningful and the census only tracks so many things. Still.
-}

type B01001_001E = "B01001_001E" F.:-> Int -- total unweighted count, our denominator
type B01001A_001E = "B01001A_001E" F.:-> Int -- W

type B01001A_002E = "B01001A_002E" F.:-> Int -- WM
F.declareColumn "WMY" '' Double
F.declareColumn "WMO" '' Double
type B01001A_003E = "B01001A_003E" F.:-> Int -- WM < 5
type B01001A_004E = "B01001A_004E" F.:-> Int -- WM 5-9
type B01001A_005E = "B01001A_005E" F.:-> Int -- WM 10-14
type B01001A_006E = "B01001A_006E" F.:-> Int -- WM 15-17
type B01001A_007E = "B01001A_007E" F.:-> Int -- WM 18-19
type B01001A_008E = "B01001A_008E" F.:-> Int -- WM 20-24
type B01001A_009E = "B01001A_009E" F.:-> Int -- WM 25-29
type B01001A_010E = "B01001A_010E" F.:-> Int -- WM 30-34
type B01001A_011E = "B01001A_011E" F.:-> Int -- WM 35-44

type WMYCodes = [B01001A_003E,B01001A_004E,B01001A_005E,B01001A_006E,B01001A_007E,B01001A_008E,B01001A_009E,B01001A_010E,B01001A_011E]
instance QueryField ACS WMY where
  type FieldCodes ACS WMY = (B01001_001E ': WMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac wmy/realToFrac tot

instance QueryField ACS WMO where
  type FieldCodes ACS WMO =  (B01001_001E ': B01001A_002E ': WMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wm = F.rgetField @B01001A_002E r
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac (wm - wmy)/realToFrac tot

type B01001A_017E = "B01001A_017E" F.:-> Int -- WF
F.declareColumn "WFY" '' Double
F.declareColumn "WFO" '' Double
type B01001A_018E = "B01001A_018E" F.:-> Int -- WF < 5
type B01001A_019E = "B01001A_019E" F.:-> Int -- WF 5-9
type B01001A_020E = "B01001A_020E" F.:-> Int -- WF 10-14
type B01001A_021E = "B01001A_021E" F.:-> Int -- WF 15-17
type B01001A_022E = "B01001A_022E" F.:-> Int -- WF 18-19
type B01001A_023E = "B01001A_023E" F.:-> Int -- WF 20-24
type B01001A_024E = "B01001A_024E" F.:-> Int -- WF 25-29
type B01001A_025E = "B01001A_025E" F.:-> Int -- WF 30-34
type B01001A_026E = "B01001A_026E" F.:-> Int -- WF 35-44
type WFYCodes = [B01001A_018E,B01001A_019E,B01001A_020E,B01001A_021E,B01001A_022E,B01001A_023E,B01001A_024E,B01001A_025E,B01001A_026E]

instance QueryField ACS WFY where
  type FieldCodes ACS WFY = (B01001_001E ': WFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac wfy/realToFrac tot

instance QueryField ACS WFO where
  type FieldCodes ACS WFO =  (B01001_001E ': B01001A_017E ': WFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wf = F.rgetField @B01001A_017E r
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac (wf - wfy)/realToFrac tot

type B01001B_002E = "B01001B_002E" F.:-> Int -- BM
F.declareColumn "BMY" '' Double
F.declareColumn "BMO" '' Double
type B01001B_003E = "B01001B_003E" F.:-> Int -- BM < 5
type B01001B_004E = "B01001B_004E" F.:-> Int -- BM 5-9
type B01001B_005E = "B01001B_005E" F.:-> Int -- BM 10-14
type B01001B_006E = "B01001B_006E" F.:-> Int -- BM 15-17
type B01001B_007E = "B01001B_007E" F.:-> Int -- BM 18-19
type B01001B_008E = "B01001B_008E" F.:-> Int -- BM 20-24
type B01001B_009E = "B01001B_009E" F.:-> Int -- BM 25-29
type B01001B_010E = "B01001B_010E" F.:-> Int -- BM 30-34
type B01001B_011E = "B01001B_011E" F.:-> Int -- BM 35-44

type BMYCodes = [B01001B_003E,B01001B_004E,B01001B_005E,B01001B_006E,B01001B_007E,B01001B_008E,B01001B_009E,B01001B_010E,B01001B_011E]
instance QueryField ACS BMY where
  type FieldCodes ACS BMY = (B01001_001E ': BMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BMYCodes r)
    in realToFrac bmy/realToFrac tot

instance QueryField ACS BMO where
  type FieldCodes ACS BMO =  (B01001_001E ': B01001B_002E ': BMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bm = F.rgetField @B01001B_002E r
        bmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BMYCodes r)
    in realToFrac (bm - bmy)/realToFrac tot

type B01001B_017E = "B01001B_017E" F.:-> Int -- BF
F.declareColumn "BFY" '' Double
F.declareColumn "BFO" '' Double
type B01001B_018E = "B01001B_018E" F.:-> Int -- BF < 5
type B01001B_019E = "B01001B_019E" F.:-> Int -- BF 5-9
type B01001B_020E = "B01001B_020E" F.:-> Int -- BF 10-14
type B01001B_021E = "B01001B_021E" F.:-> Int -- BF 15-17
type B01001B_022E = "B01001B_022E" F.:-> Int -- BF 18-19
type B01001B_023E = "B01001B_023E" F.:-> Int -- BF 20-24
type B01001B_024E = "B01001B_024E" F.:-> Int -- BF 25-29
type B01001B_025E = "B01001B_025E" F.:-> Int -- BF 30-34
type B01001B_026E = "B01001B_026E" F.:-> Int -- BF 35-44
type BFYCodes = [B01001B_018E,B01001B_019E,B01001B_020E,B01001B_021E,B01001B_022E,B01001B_023E,B01001B_024E,B01001B_025E,B01001B_026E]

instance QueryField ACS BFY where
  type FieldCodes ACS BFY = (B01001_001E ': BFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BFYCodes r)
    in realToFrac bfy/realToFrac tot

instance QueryField ACS BFO where
  type FieldCodes ACS BFO =  (B01001_001E ': B01001B_017E ': BFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bf = F.rgetField @B01001B_017E r
        bfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BFYCodes r)
    in realToFrac (bf - bfy)/realToFrac tot





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



