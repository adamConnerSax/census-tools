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

class ComputedField (b :: DataSet) (a :: (Symbol,Type)) where
  type FieldNeeds b a :: [(Symbol, Type)] -- these will be like ("B17001_001E" :: KnownSymbol,Int)
  makeField :: F.Record (FieldNeeds b a) -> V.Snd a

{-
class ComputedFields (b :: DataSet) (cfs :: [(Symbol,Type)]) where
  type FieldsNeed b cf :: [(Symbol,Type)]
  makeCRec :: F.Record (FieldsNeed b cf) -> F.Record cfs

instance ComputedFields b '[] where
  type FieldsNeed b '[] = '[]
  makeCRec _ = V.RNil

instance (V.KnownField cf, ComputedFields b cfs, ComputedField b cf, (FieldNeeds b cf) F.⊆ (FieldsNeed b (cf ': cfs)) => ComputedFields b (cf ': cfs)) where
  type FieldsNeed b (cf ': cfs) = (FieldNeeds b cf) V.++ (FieldsNeed b cfs) -- these shouldn't overlap.  We could nub them but that's v slow
  makeCRec xs =
    let newField = makeField @b @cf $ F.rcast xs
    in newField V.:& (makeCRec @b @cfs $ F.rcast xs)
-}
class QueryFields  (b :: DataSet) (as ::[(Symbol,Type)]) where
  type QueryCodes b as :: [(Symbol, Type)]
  makeQRec :: F.Record (QueryCodes b as) -> F.Record as

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
  makeQRec _ = V.RNil

instance (V.KnownField r, QueryFields b rs, ComputedField b r, (FieldNeeds b r) F.⊆ (QueryCodes b (r ': rs)), (QueryCodes b rs) F.⊆ (QueryCodes b (r ': rs))) => QueryFields b (r ': rs) where
  type QueryCodes b (r ': rs) = Nub ((FieldNeeds b r) V.++ (QueryCodes b rs)) -- don't duplicate fields.
  makeQRec qs =
    let newField = V.Field (makeField @b @r $ F.rcast qs)
    in newField V.:& (makeQRec @b @rs $ F.rcast qs)

F.declareColumn "Population" ''Int
type B01003_001E = "B01003_001E" F.:-> Int
instance ComputedField ACS Population where
  type FieldNeeds ACS Population = '[B01003_001E]
  makeField = F.rgetField @B01003_001E

F.declareColumn "MedianHouseholdIncome" ''Double
type B19013_001E = "B19013_001E" F.:-> Int
instance ComputedField ACS MedianHouseholdIncome where
  type FieldNeeds ACS MedianHouseholdIncome = '[B19013_001E]
  makeField = realToFrac . F.rgetField @B19013_001E

type SAEMHI_PT = "SAEMHI_PT" F.:-> Double
instance ComputedField SAIPE MedianHouseholdIncome where
  type FieldNeeds SAIPE MedianHouseholdIncome = '[SAEMHI_PT]
  makeField = F.rgetField @SAEMHI_PT

F.declareColumn "MedianAge" ''Double
type B01002_001E = "B01002_001E" F.:-> Double
instance ComputedField ACS MedianAge where
  type FieldNeeds ACS MedianAge = '[B01002_001E]
  makeField = F.rgetField @B01002_001E


F.declareColumn "HSGradPct" ''Double
type B06009_001E = "B06009_001E" F.:-> Int
type B06009_003E = "B06009_003E" F.:-> Int
instance ComputedField ACS HSGradPct where
  type FieldNeeds ACS HSGradPct = '[B06009_001E,B06009_003E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        hsGrads = realToFrac $ F.rgetField @B06009_003E r
    in hsGrads/tot

F.declareColumn "NonHSGradPct" ''Double
type B06009_002E = "B06009_002E" F.:-> Int
instance ComputedField ACS NonHSGradPct where
  type FieldNeeds ACS NonHSGradPct = '[B06009_001E,B06009_002E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        nonHSGrads = realToFrac $ F.rgetField @B06009_002E r
    in nonHSGrads/tot

F.declareColumn "CollegeGradPct" ''Double
type B06009_005E = "B06009_005E" F.:-> Int
instance ComputedField ACS CollegeGradPct where
  type FieldNeeds ACS CollegeGradPct = '[B06009_001E,B06009_005E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        collegeGrads = realToFrac $ F.rgetField @B06009_005E r
    in collegeGrads/tot

F.declareColumn "GradSchoolPct" ''Double
type B06009_006E = "B06009_006E" F.:-> Int
instance ComputedField ACS GradSchoolPct where
  type FieldNeeds ACS GradSchoolPct = '[B06009_001E,B06009_006E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        gradSchool = realToFrac $ F.rgetField @B06009_006E r
    in gradSchool/tot

F.declareColumn "MedianHouseholdIncomeMOE" ''Int
type SAEMHI_MOE = "SAEMHI_MOE" F.:-> Int
instance ComputedField SAIPE MedianHouseholdIncomeMOE where
  type FieldNeeds SAIPE MedianHouseholdIncomeMOE = '[SAEMHI_MOE]
  makeField = F.rgetField @SAEMHI_MOE

F.declareColumn "PovertyRate" ''Double -- PovertyR = "povertyR" :-> Double
type B17001_001E = "B17001_001E" F.:-> Int
type B17001_002E = "B17001_002E" F.:-> Int
instance ComputedField ACS PovertyRate where
  type FieldNeeds ACS PovertyRate = '[B17001_001E,B17001_002E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B17001_001E r
        below = realToFrac $ F.rgetField @B17001_002E r
    in below/tot

type SAEPOVRTALL_PT = "SAEPOVRTALL_PT" F.:-> Double
instance ComputedField SAIPE PovertyRate where
  type FieldNeeds SAIPE PovertyRate = '[SAEPOVRTALL_PT]
  makeField = F.rgetField @SAEPOVRTALL_PT

F.declareColumn "AverageHouseholdSize" ''Double
type B25010_001E = "B25010_001E" F.:-> Double
instance ComputedField ACS AverageHouseholdSize where
  type FieldNeeds ACS AverageHouseholdSize = '[B25010_001E] -- this is avg household size *of occuped housing units* which is not quite what we want
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
type B01001_002E = "B01001_002E" F.:-> Int -- total male
type B01001_003E = "B01001_003E" F.:-> Int -- M < 5
type B01001_004E = "B01001_004E" F.:-> Int -- M 5-9
type B01001_005E = "B01001_005E" F.:-> Int -- M 10-14
type B01001_006E = "B01001_006E" F.:-> Int -- M 15-17
type B01001_007E = "B01001_007E" F.:-> Int -- M 18-19
type B01001_008E = "B01001_008E" F.:-> Int -- M 20
type B01001_009E = "B01001_009E" F.:-> Int -- M 21
type B01001_010E = "B01001_010E" F.:-> Int -- M 22-24
type B01001_011E = "B01001_011E" F.:-> Int -- M 25-29
type B01001_012E = "B01001_012E" F.:-> Int -- M 30-34
type B01001_013E = "B01001_013E" F.:-> Int -- M 35-39
type B01001_014E = "B01001_014E" F.:-> Int -- M 40-44

type MYCodes = [B01001_003E, B01001_004E, B01001_005E, B01001_006E, B01001_007E, B01001_008E, B01001_009E, B01001_010E, B01001_011E, B01001_012E, B01001_013E, B01001_014E]

type B01001A_001E = "B01001A_001E" F.:-> Int -- W
type B01001A_002E = "B01001A_002E" F.:-> Int -- WM
F.declareColumn "WMY" '' Double
F.declareColumn "WMO" '' Double
F.declareColumn "NWMY" '' Double
F.declareColumn "NWMO" '' Double
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
instance ComputedField ACS WMY where
  type FieldNeeds ACS WMY = (B01001_001E ': WMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac wmy/realToFrac tot

instance ComputedField ACS WMO where
  type FieldNeeds ACS WMO =  (B01001_001E ': B01001A_002E ': WMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wm = F.rgetField @B01001A_002E r
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac (wm - wmy)/realToFrac tot

instance ComputedField ACS NWMY where
  type FieldNeeds ACS NWMY = (B01001_001E ': (WMYCodes V.++ MYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        my = Fold.foldl' (+) 0 (F.recToList $ F.rcast @MYCodes r)
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac (my - wmy)/realToFrac tot

instance ComputedField ACS NWMO where
  type FieldNeeds ACS NWMO =  (B01001_001E ': B01001_002E ': B01001A_002E ': (WMYCodes V.++ MYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        m = F.rgetField @B01001_002E r
        wm = F.rgetField @B01001A_002E r
        my = Fold.foldl' (+) 0 (F.recToList $ F.rcast @MYCodes r)
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
        mo = m - my
        wmo = wm - wmy
    in realToFrac (mo - wmo)/realToFrac tot

type B01001_026E = "B01001_026E" F.:-> Int -- total female
type B01001_027E = "B01001_027E" F.:-> Int -- F < 5
type B01001_028E = "B01001_028E" F.:-> Int -- F 5-9
type B01001_029E = "B01001_029E" F.:-> Int -- F 10-14
type B01001_030E = "B01001_030E" F.:-> Int -- F 15-17
type B01001_031E = "B01001_031E" F.:-> Int -- F 18-19
type B01001_032E = "B01001_032E" F.:-> Int -- F 20
type B01001_033E = "B01001_033E" F.:-> Int -- F 21
type B01001_034E = "B01001_034E" F.:-> Int -- F 22-24
type B01001_035E = "B01001_035E" F.:-> Int -- F 25-29
type B01001_036E = "B01001_036E" F.:-> Int -- F 30-34
type B01001_037E = "B01001_037E" F.:-> Int -- F 35-39
type B01001_038E = "B01001_038E" F.:-> Int -- F 40-44

type FYCodes =[B01001_027E, B01001_028E, B01001_029E, B01001_030E, B01001_031E, B01001_032E, B01001_033E, B01001_034E, B01001_035E, B01001_036E, B01001_037E, B01001_038E]


type B01001A_017E = "B01001A_017E" F.:-> Int -- WF
F.declareColumn "WFY" '' Double
F.declareColumn "WFO" '' Double
F.declareColumn "NWFY" '' Double
F.declareColumn "NWFO" '' Double
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

instance ComputedField ACS WFY where
  type FieldNeeds ACS WFY = (B01001_001E ': WFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac wfy/realToFrac tot

instance ComputedField ACS WFO where
  type FieldNeeds ACS WFO =  (B01001_001E ': B01001A_017E ': WFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wf = F.rgetField @B01001A_017E r
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac (wf - wfy)/realToFrac tot


instance ComputedField ACS NWFY where
  type FieldNeeds ACS NWFY = (B01001_001E ': (WFYCodes V.++ FYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        fy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @FYCodes r)
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac (fy - wfy)/realToFrac tot

instance ComputedField ACS NWFO where
  type FieldNeeds ACS NWFO =  (B01001_001E ': B01001_026E ': B01001A_017E ': (WFYCodes V.++ FYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        f = F.rgetField @B01001_026E r
        wf = F.rgetField @B01001A_017E r
        fy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @FYCodes r)
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
        fo = f - fy
        wfo = wf - wfy
    in realToFrac (fo - wfo)/realToFrac tot


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
instance ComputedField ACS BMY where
  type FieldNeeds ACS BMY = (B01001_001E ': BMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BMYCodes r)
    in realToFrac bmy/realToFrac tot

instance ComputedField ACS BMO where
  type FieldNeeds ACS BMO =  (B01001_001E ': B01001B_002E ': BMYCodes)
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

instance ComputedField ACS BFY where
  type FieldNeeds ACS BFY = (B01001_001E ': BFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BFYCodes r)
    in realToFrac bfy/realToFrac tot

instance ComputedField ACS BFO where
  type FieldNeeds ACS BFO =  (B01001_001E ': B01001B_017E ': BFYCodes)
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



